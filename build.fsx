// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.Testing.Expecto

// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------

let buildDir  = "./build/"
let appReferences = !! "/**/*.fsproj"
let testsReferences = "/**/bin/**/*ests.exe"

// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
let paketRestore mbGroup =
    let parameters = Paket.PaketRestoreDefaults()
    use __ = traceStartTaskUsing "PaketRestore" parameters.WorkingDir

    let restoreGroupArg = 
        match mbGroup with
        | Some group -> sprintf "restore --group %s" group
        | None -> "restore"

    let restoreResult =
        ExecProcess (fun info ->
            info.FileName <- parameters.ToolPath
            info.WorkingDirectory <- parameters.WorkingDir
            info.Arguments <- restoreGroupArg ) parameters.TimeOut
    if restoreResult <> 0 then failwithf "Error during restore %s." parameters.WorkingDir 

let targetBuild _ =
    MSBuildRelease "" "Build" appReferences
        |> Log "AppBuild-Output: "

let targetTests _ =
    !! testsReferences
    |> Expecto (fun p ->
        { p with
            Debug = false
            Parallel = true
            ListTests = false
            Summary = true
            FailOnFocusedTests = false
        })

// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------

Target "Clean" (fun _ ->
    CleanDirs [buildDir]
)

Target "Restore" (fun _ ->
   paketRestore None
)

Target "BuildWithoutRestore" targetBuild
Target "Build" targetBuild
Target "Tests" targetTests
Target "TestsWithoutRestore" targetTests

// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------

"Clean"
  ==> "Restore"
  ==> "Build"
  ==> "Tests"

"BuildWithoutRestore"
    ==> "TestsWithoutRestore"

RunTargetOrDefault "TestsWithoutRestore"
