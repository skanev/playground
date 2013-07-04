require 'pathname'
require 'fileutils'

require 'tilt'
require 'redcarpet'
require 'nokogiri'

require_relative 'lib/catalog'
require_relative 'lib/exercise'
require_relative 'lib/problem'
require_relative 'lib/renderer'
require_relative 'lib/generator'

SOLUTION_ROOT = Pathname(__FILE__).dirname.join('..').expand_path
VIEWS_ROOT    = Pathname(__FILE__).dirname.join('views/').expand_path
