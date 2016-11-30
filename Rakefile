task :default => :test

code = FileList.new "*/**.hs", "*/**.lhs"

desc "Build the program"
task :build do
  verbose(false) { sh "stack build" }
end

desc "Run the program"
task :run => :build do
  verbose(false) { sh "stack exec bombastic 2>&1" }
end

desc "Test the program"
task :test do
  verbose(false) { sh "stack test 2>&1" }
end

desc "Lint the program"
task :lint do
  verbose(true) { sh "hlint #{code}" }
end

desc "Automatically apply lintings"
task :lintapply do
  code.each do |f|
    verbose(false) { sh "hlint --refactor --refactor-options=-is #{f}" }
  end
end
