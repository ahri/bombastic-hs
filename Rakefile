task :default => :test

desc "Build the program"
task :build do
  verbose(false) { sh "stack build" }
end

desc "Run the program"
task :run do
  verbose(false) { sh "stack runhaskell app/Main.hs -- -Wall -Werror 2>&1" }
end

desc "Test the program"
task :test do
  verbose(false) { sh "stack test 2>&1" }
end

desc "Lint the program"
task :lint do
  verbose(false) { sh "hlint src/Main.hs; true" }
end

desc "Automatically apply lintings"
task :lintapply do
  verbose(false) { sh "hlint --refactor --refactor-options=-is src/Main.hs" }
end
