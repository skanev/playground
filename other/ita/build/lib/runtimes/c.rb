module Runtimes
  module C
    extend self

    def compile_and_run_test(test_path, target_path)
      target_path = Pathname(target_path)
      test_path   = Pathname(test_path)

      target_path.dirname.mkpath
      runner_code_path = target_path.sub_ext('.c')

      write_test_file test_path, runner_code_path
      compile_runner runner_code_path, target_path
      run target_path
    end

    private

    def write_test_file(test_path, runner_path)
      tests = File.read(test_path).scan(/^\W*TEST\((\w+)/).flatten

      File.open runner_path, 'w' do |file|
        file.write <<-END
#include "#{test_path.expand_path.to_s}"

int main() {
    test_initialize();
END
        tests.each do |name|
          file.write "    run_#{name}();"
        end

        file.write <<-END
    return test_report_results();
}
        END
      end
    end

    def compile_runner(runner_code_path, target_path)
      opts    = %W[cc -std=c99 #{runner_code_path.to_s} -o #{target_path.to_s}]
      success = system(*opts)

      raise "Failed to compile #{runner_code_path.to_s}" unless success
    end

    def run(target_path)
      system target_path.to_s
    end
  end
end
