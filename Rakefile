task :default => :test

code = FileList.new "*/**.hs", "*/**.lhs"
production_code = FileList.new "app/**.hs", "src/**.hs", "app/*.lhs", "src/*.lhs"

desc "Build the program"
task :build do
  verbose(false) { sh "stack build" }
end

desc "Interactive debug"
task :repl do
  verbose(false) { sh "stack ghci" }
end

desc "Run the program"
task :run => :build do
  verbose(false) { sh "stack exec bombastic 2>&1" }
end

desc "Test the program"
task :test do
  verbose(false) { sh "stack test 2>&1" }
end

desc "Watch the tests for the program"
task :watch do
  sh "stack build --file-watch --test"
end

desc "Lint the program"
task :lint do
  verbose(true) { sh "hlint #{production_code}" }
end

desc "Automatically apply lintings"
task :lintapply do
  production_code do |f|
    verbose(false) { sh "hlint --refactor --refactor-options=-is #{f}" }
  end
end
