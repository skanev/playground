module Runtimes
  module Python
    extend self

    def run(location, capture_output = false)
      path = SOLUTION_ROOT.join(location).sub_ext '.run.py'
      execute path, capture_output
    end

    def run_test(location, capture_output = false)
      path = SOLUTION_ROOT.join(location).sub_ext '.test.py'
      execute path, capture_output
    end

    private

    def execute(path, capture_output = false)
      if capture_output
        result = `python3 #{path.to_s} 2>&1`
        raise if $?.exitstatus != 0
        result
      else
        system 'python3', path.to_s
        raise if $?.exitstatus != 0
      end
    end
  end
end
