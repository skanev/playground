watch(%r{^(\d+)/(\d+).scm$})             { |m| racket_test m[1], m[2] if File.exists? "#{m[1]}/tests/#{m[2]}-tests.scm" }
watch(%r{^(\d+)/tests/(\d+)-tests.scm$}) { |m| racket_test m[1], m[2] }

def racket_test(chapter, exercise)
  system 'clear'
  system "rake run:exercise[#{chapter},#{exercise}]"
end
