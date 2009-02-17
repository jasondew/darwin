desc "compile Darwin"
task :compile do
  `ghc -O2 --make Darwin.hs`
end

desc "compile Darwin for profiling"
task :compile_with_profiling do
  `ghc -O2 --make Darwin.hs -prof -auto-all -caf-all -fforce-recomp`
end

desc "run Darwin"
task :run => :compile do
  system "time ./Darwin monalisa.jpg"
end

desc "profile Darwin"
task :profile => :compile_with_profiling do
  system "time ./Darwin -i 3000 monalisa.jpg +RTS -p"
end

desc "remove iteration images and compiled code"
task :clean do
  `rm iteration*.png`
  `rm Darwin Darwin.hi Darwin.o`
end

desc "clean up and run Darwin"
task :default => [:clean, :compile, :run]
