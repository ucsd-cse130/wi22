


main :: IO ()
main = putStrLn "Hello, 130!"
IO
-- type Recipe a = IO a

-- main :: Recipe (

main :: IO ()
main = combine sayHello sayBye

combine :: () -> IO () -> IO ()

hellos = [sayHello, sayBye]
hell2 = (sayHello, sayBye)

sayHello :: IO ()
sayHello = putStrLn "Hello hello, world!"

sayBye :: IO ()
sayBye   = putStrLn "Bye Bye, world!"
