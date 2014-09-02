import Data.IORef
import Data.Bits
import Text.Printf
import System.Random (randomRIO)
import qualified Data.ByteString.Lazy as BS
import Data.Word
import Data.Bits

import Tools

data Memory = Memory {
    memoryPc :: IORef Int,
    memoryOpCode :: IORef Int,
    memoryI :: IORef Int,
    memorySp :: IORef Int,
    memoryDelayTimer :: IORef Int,
    memorySoundTimer :: IORef Int,
    memoryGfx :: IORef [Int],
    memoryStack :: IORef [Int],
    memoryKey :: IORef [Int],
    memoryV :: IORef [Int],
    memoryMemory :: IORef [Int],
    memoryDrawFlag :: IORef Bool
}

readBinary :: String -> IO [Int]
readBinary fp = (BS.readFile fp) >>= (map fromIntegral) . (BS.unpack)

initialize :: IO Memory 
initialize = do
    pc <- newIORef 0x200 
    opcode <- newIORef 0
    i_ <- newIORef 0
    sp <- newIORef 0
    delay_timer <- newIORef 0
    sound_timer <- newIORef 0
    gfx <- newIORef [0 | _ <- [0..2047]]
    stack <- newIORef [0 | _ <- [0..15]]
    key <- newIORef [0 | _ <- [0..15]]
    v_ <- newIORef [0 | _ <- [0..15]]
    memory <- newIORef [0 | _ <- [0..4095]]
    draw_flag <- newIORef True
    return $ Memory pc opcode i_ sp delay_timer sound_timer gfx stack key v_ memory draw_flag



loadApplication :: String -> IO Memory
loadApplication filepath = do
    memory <- initialize
    readBinary filepath >>= \pFile ->
        if length pFile < (4096-512) then do
            writeIORef (memoryMemory memory) ([0 | _ <- [0..512]]++pFile++[0 | _ <- [0..(4090-(length pFile)-512)]])
        else
            fail "Unvalid file length"
    return memory

emulateCycle :: Memory -> IO ()
emulateCycle m = do
    pc <- readIORef (memoryPc m)
    exFunction (pc `shiftL` 8 .|. (pc + 1))
    where
        exFunction opcode = do
            v_ <- readIORef (memoryV m)
            i_ <- readIORef (memoryI m)
            case opcode .&. 0xF000 of 
                0x0000 ->
                    case opcode .&. 0x000F of 
                        0x0000 -> do
                            writeIORef (memoryGfx m) [0 | _ <- [0..2047]]
                            writeIORef (memoryDrawFlag m) True
                            modifyIORef (memoryPc m) (+2)
                        0x000E -> do
                            stack <- readIORef (memoryStack m)
                            sp <- readIORef (memorySp m)
                            modifyIORef (memoryPc m) (+(-1))
                            writeIORef (memoryPc m) ((stack !! sp)+2)
                        otherwise ->
                            fail $ printf "Unknown opcode [0x0000]: 0x%X\n" opcode
                0x1000 ->
                    writeIORef (memoryPc m) (opcode .&. 0x0FFF)
                0x2000 -> do
                    sp <- readIORef (memorySp m)
                    pc <- readIORef (memoryPc m)
                    modifyElemIORef (memoryStack m) sp pc
                    modifyIORef (memorySp m) (+1)
                    writeIORef (memoryPc m) (opcode .&. 0x0FFF)
                0x3000 -> do
                    if v_ !! ((opcode .&. 0x0F00) `shiftR` 8) == (opcode .&. 0x00FF) then
                        modifyIORef (memoryPc m) (+4)
                    else
                        modifyIORef (memoryPc m) (+2)
                0x4000 -> do
                    if v_ !! ((opcode .&. 0x0F00) `shiftR` 8) /= (opcode .&. 0x00FF) then
                        modifyIORef (memoryPc m) (+4)
                    else
                        modifyIORef (memoryPc m) (+2)
                0x5000 -> do
                    if v_ !! ((opcode .&. 0x0F00) `shiftR` 8) == v_ !! ((opcode .&. 0x00F0) `shiftR` 4) then
                        modifyIORef (memoryPc m) (+4)
                    else
                        modifyIORef (memoryPc m) (+2)
                0x6000 -> do
                    modifyElemIORef (memoryV m) (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) (opcode .&. 0x00FF)
                    modifyIORef (memoryPc m) (+2)
                0x7000 -> do
                    writeIORef (memoryV m) [if i == (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) then ((v_ !! i) + (opcode .&. 0x00FF)) else v_ !! i | i <- [0..(length v_)-1]]
                    modifyIORef (memoryPc m) (+2)
                0x8000 -> 
                    case opcode .&. 0x00F of
                        0x0000 -> do
                            writeIORef (memoryV m) $ replace (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) ((v_ !! (opcode .&. 0x00F0)) `shiftR` 4) v_
                            modifyIORef (memoryPc m) (+2)
                        0x0001 -> do
                            writeIORef (memoryV m) $ replace (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) v_
                            modifyIORef (memoryPc m) (+2)
                        0x0002 -> do
                            writeIORef (memoryV m) $ replace (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) ((v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) .&. ((v_ !! (opcode .&. 0x00F0)) `shiftR` 4) `shiftR` 4) v_
                            modifyIORef (memoryPc m) (+2)
                        0x0003 -> do
                            writeIORef (memoryV m) $ replace (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) ((v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) `xor` ((v_ !! (opcode .&. 0x00F0)) `shiftR` 4) `shiftR` 4) v_
                            modifyIORef (memoryPc m) (+2) 
                        0x0004 -> do
                            if ((v_ !! (opcode .&. 0x00F0)) `shiftR` 4) > (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) then
                                writeIORef (memoryV m) $ replace 0xF 1 v_
                            else
                                writeIORef (memoryV m) $ replace 0xF 0 v_
                            writeIORef (memoryV m) $ replace (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) ((v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) + ((v_ !! (opcode .&. 0x00F0)) `shiftR` 4)) v_
                            modifyIORef (memoryPc m) (+2)
                        0x0005 -> do
                            if ((v_ !! (opcode .&. 0x00F0)) `shiftR` 4) > (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) then
                                writeIORef (memoryV m) $ replace 0xF 1 v_
                            else
                                writeIORef (memoryV m) $ replace 0xF 0 v_
                            writeIORef (memoryV m) $ replace (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) ((v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) - ((v_ !! (opcode .&. 0x00F0)) `shiftR` 4)) v_
                            modifyIORef (memoryPc m) (+2)
                        0x0006 -> do
                            writeIORef (memoryV m) $ replace 0xF ((v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) `shiftR` 7) v_
                            writeIORef (memoryV m) $ replace (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) ((v_ !! ((opcode .&. 0x0F00) `shiftR` 8) `shiftR` 1)) v_
                            modifyIORef (memoryPc m) (+2)
                        0x0007 ->  do
                            if ((v_ !! (opcode .&. 0x00F0)) `shiftR` 4) > (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) then
                                writeIORef (memoryV m) $ replace 0xF 1 v_
                            else
                                writeIORef (memoryV m) [if i == 0xF then 0 else v_ !! i | i <- [0..(length v_)-1]]
                            writeIORef (memoryV m) [if i == (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) then (((v_ !! (opcode .&. 0x00F0)) `shiftR` 4) - (v_ !! ((opcode .&. 0x0F00) `shiftR` 8))) else v_ !! i | i<- [0..(length v_)-1]]
                            modifyIORef (memoryPc m) (+2)
                        0x000E -> do
                            writeIORef (memoryV m) [if i == 0xF then ((v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) `shiftR` 7) else v_ !! i | i <- [0..(length v_)-1]]
                            writeIORef (memoryV m) [if i == (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) then ((v_ !! ((opcode .&. 0x0F00) `shiftR` 8) `shiftL` 1)) else v_ !! i | i<- [0..(length v_)-1]]
                            modifyIORef (memoryPc m) (+2)
                        otherwise ->
                            fail $ printf "Unknown opcode [0x8000]: 0x%X\n" opcode  
                0x9000 -> do
                    if v_ !! ((opcode .&. 0x0F00) `shiftR` 8) /= v_ !! ((opcode .&. 0x00F0) `shiftR` 4) then
                        modifyIORef (memoryPc m) (+4)
                    else
                        modifyIORef (memoryPc m) (+2)
                0xA000 -> do
                    writeIORef (memoryI m) (opcode .&. 0x0FFF)
                    modifyIORef (memoryPc m) (+2)
                0xB000 ->
                    readIORef (memoryV m) >>= \v_ -> writeIORef (memoryPc m) ((opcode .&. 0x0FFF) + (v_ !! 0))
                0xC000 -> do
                    random <- randomRIO (0x0, 0xFF)
                    writeIORef (memoryV m) [if i == (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) then random .&. (opcode .&. 0x00FF) else v_ !! i | i<- [0..(length v_)-1]]
                    modifyIORef (memoryPc m) (+2)
                0xD000 -> do
                    
                    mem <- readIORef (memoryMemory m)
                    gfx <- readIORef (memoryGfx m)
                    writeIORef (memoryV m) [if i == 0xF then (if (1 `elem` gfx) then 1 else 0) else v_ !! 1 | i <- [0..length v_-1]]
                    let lst = [[(x + xline + ((y + yline) * 64)) | xline <- [0..7], ((mem !! (i_ + yline)) .&. (0x80 `shiftR` xline))] | yline <- [0..height-1]] in
                        writeIORef (memoryGfx m) [(gfx !! i) `xor` 1 | i <- lst]
                    writeIORef (memoryV m) [if i == 0xF then 0 else v_ !! i | i <- [0..(length v_)-1]]  
                    writeIORef (memoryDrawFlag m) True
                    modifyIORef (memoryPc m) (+2)
                    where
                        height = opcode .&. 0x000F
                        x = ((opcode .&. 0x0F00) `shiftR` 8)
                        y = ((opcode .&. 0x00F0) `shiftR` 4)
                0xE000 ->
                    case opcode .&. 0x00FF of
                        0x009E -> do
                            key <- readIORef (memoryKey m)
                            if key !! (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) /= 0 then
                                modifyIORef (memoryPc m) (+4)
                            else
                                modifyIORef (memoryPc m) (+2)
                        0x00A1 -> do
                            key <- readIORef (memoryKey m)
                            if key !! (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) == 0 then
                                modifyIORef (memoryPc m) (+4)
                            else
                                modifyIORef (memoryPc m) (+2)
                        otherwise ->
                            fail $ printf "Unknown opcode [0xE000]: 0x%X\n" opcode  
                0xF000 ->
                    case opcode .&. 0x00FF of
                        0x0007 -> do
                            delay_timer <- readIORef (memoryDelayTimer m)
                            writeIORef (memoryV m) [if i == (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) then delay_timer else v_ !! i | i<- [0..(length v_)-1]]
                        0x000A -> do
                            key <- readIORef (memoryKey m)
                            if length [i | i <- [0..length key -1]] > 0 then do
                                writeIORef (memoryV m) ([if i == (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) then (maximum [i | i <- [0..length key -1]]) else v_ !! i | i<- [0..(length v_)-1]])
                                modifyIORef (memoryPc m) (+2)
                            else do
                                modifyIORef (memoryPc m) (+0)
                        0x0015 -> do
                            writeIORef (memoryDelayTimer m) (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) 
                            modifyIORef (memoryPc m) (+2)
                        0x0018 -> do
                            writeIORef (memorySoundTimer m) (v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) 
                            modifyIORef (memoryPc m) (+2)
                        0x001E -> do
                            
                            if (i_ + (v_ !! ((opcode .&. 0x0F00) `shiftR` 8))) > 0xFFF then
                                writeIORef (memoryV m) [if i == 0xF then 1 else v_ !! i | i <- [0..(length v_)-1]]
                            else
                                writeIORef (memoryV m) [if i == 0xF then 0 else v_ !! i | i <- [0..(length v_)-1]]
                            writeIORef (memoryI m) (v_ !! ((opcode .&. 0x0F00) `shiftR` 8))
                            modifyIORef (memoryPc m) (+2)
                        0x0029 -> do
                            writeIORef (memoryI m) ((v_ !! ((opcode .&. 0x0F00) `shiftR` 8)) * 0x5)
                            modifyIORef (memoryPc m) (+2)
                        0x0033 -> do
                            mem <- readIORef (memoryMemory m)
                            
                            writeIORef (memoryMemory m) [if i == i_ then (gv v_) / 100 else mem !! i | i<- [0..(length mem)-1]]
                            writeIORef (memoryMemory m) [if i == i_ + 1 then ((gv v_) / 100) `mod` 10 else mem !! i | i<- [0..(length mem)-1]]
                            writeIORef (memoryMemory m) [if i == i_ + 1 then ((gv v_) `mod` 100) `mod` 10 else mem !! i | i<- [0..(length mem)-1]]
                            modifyIORef (memoryPc m) (+2)
                            where
                                gv = (v_ !! ((opcode .&. 0x0F00) >> 8))
                        0x0055 -> do
                            mem <- readIORef (memoryMemory m)
                            
                            writeIORef (memoryMemory m) [if (i >= i_) && (i < (i_ + length v_)) then v_ !! (i - i_) else mem !! i | i <- [0..length mem-1]]
                            modifyIORef (memoryI m) (+(((opcode .&. 0x0F00) `shiftR` 8)+1))
                            modifyIORef (memoryPc m) (+2)
                        0x0065 -> do
                            mem <- readIORef (memoryMemory m)
                            
                            writeIORef (memoryV m) [if i <= ((opcode .&. 0x0F00) `shiftR` 8) then mem !! (i_ + i) else v_ !! i |  i <- [0..length v_-1]] 
                            modifyIORef (memoryI m) (+(((opcode .&. 0x0F00) `shiftR` 8)+1))
                            modifyIORef (memoryPc m) (+2)
                        otherwise ->
                            fail $ printf "Unknown opcode [0xF000]: 0x%X\n" opcode
                otherwise ->
                    fail $ printf "Unknown opcode: 0x%X\n" opcode