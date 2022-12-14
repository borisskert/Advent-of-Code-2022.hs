module Day10.CPU (CPU, simple, load, executeUntil, signalStrength) where

import Day10.Instruction (Instruction)
import qualified Day10.Instruction as Instruction (cycles, execute)
import Day10.Program

data CPU = CPU {x :: Int, counter :: Int, program :: Program}

simple :: CPU
simple = CPU {x = 1, counter = 0, program = empty}

load :: Program -> CPU -> CPU
load programToLoad CPU {x = cpuX, counter = cpuCounter} = CPU {x = cpuX, counter = cpuCounter, program = programToLoad}

executeUntil :: Int -> CPU -> CPU
executeUntil count cpu@CPU {counter = cpuCounter, program = loadedProgram}
  | count <= 0 = cpu
  | count <= neededCycles = CPU {x = x cpu, counter = cpuCounter + count, program = loadedProgram}
  | otherwise = executeUntil (count - neededCycles) newCpu
  where
    instruction = current loadedProgram
    neededCycles = Instruction.cycles instruction
    newCpu = execute instruction cpu

execute :: Instruction -> CPU -> CPU
execute instruction CPU {x = registerX, counter = cpuCounter, program = cpuProgram} =
  (CPU {x = changedX, counter = newCounter, program = remaining cpuProgram})
  where
    changedX = Instruction.execute registerX instruction
    newCounter = cpuCounter + Instruction.cycles instruction

signalStrength :: CPU -> Int
signalStrength CPU {x = x', counter = cpuCounter} = cpuCounter * x'
