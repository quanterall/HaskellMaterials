# Example of handling union types & record types together

What follows is the full example of the union & record example in the document on
[Composite Datatypes](./02-composite-datatypes.md). Try saving it as `Example.hs` and running it:

```bash
$ stack script --resolver lts-16.31 Example.hs
Using resolver: lts-16.31 specified on command line
Kristina is a 24 years old backend programmer at Quanterall
Pesho is a 42 years old frontend programmer at Quanterall
Sasho is a 18 years old fullstack programmer at Quanterall
John Atanasoff was a professor of theoretical physics
```

It can also be very instructive to modify and add more information to see how it affects compilation
and the running of the program. As an example, you could try adding more professions as well as imbue
the `Dead` case of `Age` with either a year of death or the age someone died at.

```haskell
data Person = Person
  { personName :: String,
    personAge :: Age,
    personProfession :: Profession
  }

-- These are here only to distinguish "normal strings" from these particular types of strings.
-- `newtype` doesn't actually create a new type for runtime, but exists only as a distinction
-- between the types for compile-time.
newtype CompanyName = CompanyName String
newtype FictionalWorkName = FictionalWorkName String

data Age
  = Dead
  | Living Int

-- Our union cases can care about wildly different things.
data Profession
  = Programmer ProgrammerRole CompanyName
  | Professor Subject
  | Student Subject
  | FictionalCharacter FictionalCharacter
  | Unemployed

data ProgrammerRole
  = Backend
  | Frontend
  | Fullstack

data Subject
  = Finance
  | Physics AppliedOrTheoretical
  | Mathematics AppliedOrTheoretical
  | ComputerScience
  | ComputerEngineering

data AppliedOrTheoretical = Applied | Theoretical

data FictionalCharacter
  = Protagonist FictionalWorkName
  | Antagonist FictionalWorkName

printPerson :: Person -> String
printPerson
  Person
    { personName = name,
      personAge = Living age,
      personProfession = profession
    } =
    name <> " is a " <> show age <> " years old " <> printProfession profession
printPerson
  Person
    { personName = name,
      personAge = Dead,
      personProfession = profession
    } =
    name <> " was a " <> printProfession profession

printProfession :: Profession -> String
printProfession Unemployed = "unemployed person"
printProfession (Professor subject) = "professor of " <> printSubject subject
printProfession (Student subject) = "student of " <> printSubject subject
printProfession (FictionalCharacter fictionRole) = printFictionRole fictionRole
printProfession (Programmer role (CompanyName name)) =
  printProgrammerRole role <> " programmer at " <> name

printSubject :: Subject -> String
-- Note here how we are matching on two different physics & mathematics cases, one for applied
-- and one for theoretical.
printSubject (Physics Applied) = "applied physics"
printSubject (Physics Theoretical) = "theoretical physics"
printSubject (Mathematics Applied) = "applied mathematics"
printSubject (Mathematics Theoretical) = "theoretical mathematics"
printSubject ComputerScience = "computer science"
printSubject ComputerEngineering = "computer engineering"
printSubject Finance = "some finance thing"

printFictionRole :: FictionalCharacter -> String
printFictionRole (Protagonist (FictionalWorkName name)) =
  "protagonist of '" <> name <> "'"
printFictionRole (Antagonist (FictionalWorkName name)) =
  "antagonist of '" <> name <> "'"

printProgrammerRole :: ProgrammerRole -> String
printProgrammerRole Backend = "backend"
printProgrammerRole Frontend = "frontend"
printProgrammerRole Fullstack = "fullstack"

aBackendProgrammer :: Person
aBackendProgrammer =
  Person
    { personName = "Kristina",
      personAge = Living 24,
      personProfession =
        Programmer Backend (CompanyName "Quanterall")
    }

aFrontendProgrammer :: Person
aFrontendProgrammer =
  Person
    { personName = "Pesho",
      personAge = Living 42,
      personProfession =
        Programmer Frontend (CompanyName "Quanterall")
    }

aFullstackProgrammer :: Person
aFullstackProgrammer =
  Person
    { personName = "Sasho",
      personAge = Living 18,
      personProfession =
        Programmer Fullstack (CompanyName "Quanterall")
    }

aProfessor :: Person
aProfessor =
  Person
    { personName = "John Atanasoff",
      personAge = Dead,
      personProfession = Professor (Physics Theoretical)
    }

main :: IO ()
main = do
  putStrLn $ printPerson aBackendProgrammer
  putStrLn $ printPerson aFrontendProgrammer
  putStrLn $ printPerson aFullstackProgrammer
  putStrLn $ printPerson aProfessor
```
