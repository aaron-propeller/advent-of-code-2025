import System.Environment (getArgs)
import AoCRunner.AoCCLI (parseArgs, showUsage, AoCCommand(..))
import AoCRunner.AoCRegistry (availableDays)
import AoCRunner.AoCExecution (executeDayBoth, executeDaySingle, executeDaySample)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs availableDays args of
    Right (RunBoth day) -> executeDayBoth day
    Right (RunSample day) -> executeDaySample day
    Right (RunSingle day inputFile) -> executeDaySingle day inputFile
    Left _ -> showUsage availableDays
