import Lake
open Lake DSL System

def libcurlLink : String :=
  if Platform.isOSX then
    "/opt/homebrew/opt/curl/lib/libcurl.dylib"
  else
    "-lcurl"

package «lean-otel» where
  version := v!"0.1.0"

require Curl from git "https://github.com/bergmannjg/leanCurl"@"main"

@[default_target] lean_lib LeanOtel

lean_exe «lean-otel» where
  root := `Main
  moreLinkArgs := #[libcurlLink]

lean_exe «lean-otel-test» where
  root := `Test
  moreLinkArgs := #[libcurlLink]
