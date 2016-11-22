task :default => :compile

desc "Compile the program"
task :compile do
  verbose(false) { sh "stack runhaskell src/Main -- -Wall -Werror 2>&1" }
end

desc "Lint the program"
task :lint do
  verbose(false) { sh "hlint src/Main.hs" }
end

desc "Automatically apply lintings"
task :lintapply do
  verbose(false) { sh "hlint --refactor --refactor-options='-is' src/Main.hs" }
end
