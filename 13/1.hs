import Base

comment :: Parser ()
comment = do string "--"
             many (sat (/= '\n'))
             return ()

