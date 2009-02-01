desc "compile Darwin"
task :compile do
  `ghc -O2 --make Darwin.hs`
end

desc "run Darwin"
task :run do
  system "time ./Darwin"
end

desc "clear all iteration images"
task :clear do
  `rm iteration*.jpg`
end

desc "clean up and run Darwin"
task :default => [:clear, :compile, :run]
