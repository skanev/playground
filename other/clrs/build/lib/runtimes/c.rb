module Runtimes
  module C
    extend self

    def run_test(location, capture_output = false)
      test_path   = SOLUTION_ROOT.join(location).sub_ext('.test.c')
      target_path = SOLUTION_ROOT.join('target/bin').join(location).sub_ext('.test')
      runner_path = target_path.sub_ext('.c')

      write_test_file test_path, runner_path
      compile runner_path, target_path
      execute target_path, capture_output
    end

    def run(location, capture_output = false)
      runner_path = SOLUTION_ROOT.join(location).sub_ext('.run.c')
      target_path = SOLUTION_ROOT.join('target/bin').join(location)

      compile runner_path, target_path
      execute target_path, capture_output
    end

    private

    def write_test_file(test_path, runner_path)
      test_names = File.read(test_path).scan(/^\W*TEST\((\w+)/).flatten

      runner_path.dirname.mkpath

      File.open runner_path, 'w' do |file|
        file.write <<-END
#include "#{test_path.expand_path.to_s}"

int main() {
    test_initialize();
END
        test_names.each do |name|
          file.write "    run_#{name}();"
        end

        file.write <<-END
    return test_report_results();
}
        END
      end
    end

    def compile(code_path, target_path)
      target_path.dirname.mkpath

      opts    = %W[cc -std=c99 #{code_path.to_s} -o #{target_path.to_s}]
      success = system(*opts)

      raise "Failed to compile #{code_path.to_s}" unless success
    end

    def execute(executable_path, capture_output)
      if capture_output
        `#{executable_path.to_s} 2>&1`
      else
        system executable_path.to_s
      end
    end
  end
end
