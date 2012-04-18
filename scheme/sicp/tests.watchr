watch(%r{^(\d+)/(\d+).scm$})             { |m| run_file m[1], m[2] }
watch(%r{^(\d+)/tests/(\d+)-tests.scm$}) { |m| racket_test m[1], m[2] }

def run_file(chapter, exercise)
  if File.exists? "#{chapter}/tests/#{exercise}-tests.scm"
    racket_test chapter, exercise
  else
    system 'clear'
    system "rake run:exercise[#{chapter},#{exercise}]"
  end
end

def racket_test(chapter, exercise)
  system 'clear'
  system "rake run:test[#{chapter},#{exercise}]"
end
