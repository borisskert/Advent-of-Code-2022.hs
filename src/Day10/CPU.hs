module Day10.CPU (CPU, simple, load, ticks, tick, run, isIdle, signalStrength, register) where

import Common.Fold (times)
import qualified Day10.Instruction as Instruction (cycles, execute)
import Day10.Program

data CPU = CPU {x :: Int, cycles :: Int, counter :: Int, program :: Program}

simple :: CPU
simple = CPU {x = 1, cycles = 0, counter = 0, program = empty}

load :: Program -> CPU -> CPU
load programToLoad CPU {x = cpuX, cycles = cpuCycles, counter = cpuCounter} =
  CPU {x = cpuX, cycles = cpuCycles, counter = cpuCounter, program = programToLoad}

ticks :: Int -> CPU -> CPU
ticks n cpu = times tick cpu n

tick :: CPU -> CPU
tick CPU {x = cpuX, cycles = cpuCycles, counter = cpuCounter, program = loadedProgram}
  | cpuCycles >= neededCycles = CPU {x = changedX, cycles = 1, counter = cpuCounter + 1, program = remaining loadedProgram}
  | otherwise = CPU {x = cpuX, cycles = cpuCycles + 1, counter = cpuCounter + 1, program = loadedProgram}
  where
    instruction = current loadedProgram
    neededCycles = Instruction.cycles instruction
    changedX = Instruction.execute cpuX instruction

run :: CPU -> CPU
run cpu@CPU {program = loadedProgram}
 | isFinished loadedProgram = cpu
 | otherwise = run . tick $ cpu

isIdle :: CPU -> Bool
isIdle cpu@CPU {program = loadedProgram} = isFinished loadedProgram

signalStrength :: CPU -> Int
signalStrength CPU {x = x', counter = cpuCounter} = cpuCounter * x'

register :: CPU -> Int
register CPU {x = cpuX} = cpuX
