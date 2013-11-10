require 'pathname'
require 'fileutils'

require 'tilt'
require 'redcarpet'
require 'nokogiri'
require 'coderay'

require_relative 'lib/chapter_number'
require_relative 'lib/catalog'
require_relative 'lib/solution'
require_relative 'lib/exercise'
require_relative 'lib/problem'
require_relative 'lib/renderer'
require_relative 'lib/generator'
require_relative 'lib/graph'
require_relative 'lib/runtimes'
require_relative 'lib/runtimes/c'
require_relative 'lib/runtimes/python'

SOLUTION_ROOT = Pathname(__FILE__).dirname.join('..').expand_path
VIEWS_ROOT    = Pathname(__FILE__).dirname.join('views/').expand_path
