guard :shell, :all_on_start => true do
  watch %r{^(Rakefile|src/.*\.l?hs)$} do |m|
    puts ">>> #{m[0]} changed"
    puts `rake`
    puts ">>> Done!"
  end
end
