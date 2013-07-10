require 'sinatra'
require 'pathname'

require_relative 'build'

set :root, File.dirname(__FILE__)
set :public_folder, Proc.new { File.join(root, 'public') }
set :views, Proc.new { File.join(root, 'views') }

get '/' do
  catalog = Catalog.new SOLUTION_ROOT
  Renderer.render_catalog catalog
end

get '/index.?:format?' do
  catalog = Catalog.new SOLUTION_ROOT
  Renderer.render_catalog catalog
end

get '/:chapter/problems/:number.?:format?' do
  problem = Problem.new params[:chapter], params[:number]
  Renderer.render_problem problem
end

get '/:chapter/:section/:number.png' do
  exercise = Exercise.new params[:chapter], params[:section], params[:number]

  content_type 'image/png'
  Graph.render exercise.graph_file
end

get '/:chapter/:section/:number.?:format?' do
  exercise = Exercise.new params[:chapter], params[:section], params[:number]
  Renderer.render_exercise exercise
end

