
program prova
CHARACTER(LEN=4) :: Title
INTEGER          :: DrMD = 0, PhD = 0, MS = 0, BS = 0, Others = 0

SELECT CASE (Title)
   CASE ("DrMD")
      DrMD = DrMD + 1
   CASE ("PhD")
      PhD = PhD + 1
   CASE ("MS")
      MS = MS + 1
   CASE ("BS")
      BS = BS + 1
   CASE DEFAULT
      Others = Others + 1
   END SELECT 
END PROGRAM prova
