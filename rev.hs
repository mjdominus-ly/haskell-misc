import Argv

main :: IO ()
main = do
    input >>= mapM_ putStrLn . map reverse . lines