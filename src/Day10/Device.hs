module Day10.Device (Device, crtDevice, load, ticks, tick, run, signalStrength, screenOutput) where

import Common.Fold (times)
import Day10.CPU (CPU, register, simple, isIdle)
import qualified Day10.CPU as CPU (load, signalStrength, tick)
import Day10.Program (Program)
import Day10.Screen

data Device = Device {cpu :: CPU, screen :: Screen, cycles :: Int}

crtDevice :: Device
crtDevice = Device {cpu = simple, screen = empty, cycles = 0}

load :: Program -> Device -> Device
load program Device {cpu = deviceCPU, screen = deviceScreen, cycles = deviceCycles} =
  Device {cpu = CPU.load program deviceCPU, screen = deviceScreen, cycles = deviceCycles}

ticks :: Int -> Device -> Device
ticks n device = times tick device n

tick :: Device -> Device
tick Device {cpu = deviceCPU, screen = deviceScreen, cycles = deviceCycles} =
  Device {cpu = nextCpu, screen = nextScreen, cycles = deviceCycles + 1}
  where
    nextCpu = CPU.tick deviceCPU
    spritePosition = register nextCpu
    nextScreen = draw spritePosition deviceScreen

run :: Device -> Device
run device@Device {cpu = deviceCPU}
  | isIdle deviceCPU = device
  | otherwise = run . tick $ device

signalStrength :: Device -> Int
signalStrength Device {cpu = deviceCPU} = CPU.signalStrength deviceCPU

screenOutput :: Device -> String
screenOutput Device {screen = deviceScreen} = toLines deviceScreen
