desc "compile Darwin"
task :compile do
  `ghc -O2 --make Darwin.hs`
end

desc "compile Darwin for profiling"
task :compile_with_profiling do
  `ghc -O2 --make Darwin.hs -prof -auto-all -caf-all -fforce-recomp`
end

desc "run Darwin"
task :run do
  system "time ./Darwin monalisa.jpg"
end

desc "remove iteration images and compiled code"
task :clean do
  `rm iteration*.png`
  `rm Darwin Darwin.hi Darwin.o`
end

desc "clean up and run Darwin"
task :default => [:clean, :compile, :run]
