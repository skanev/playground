desc 'Install git hooks'
task :hooks do
  Dir['git-hooks/*'].each do |file|
    name = file.split('/').last
    sh "ln -s ../../#{file} .git/hooks/#{name}"
  end
end
