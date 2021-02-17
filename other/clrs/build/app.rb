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

get '/:chapter/problems/:number.png' do
  problem = Problem.new ChapterNumber.new(params[:chapter]), params[:number]

  content_type 'image/png'
  Graph.render_png problem.graph_path
end

get '/:chapter/problems/:number.svg' do
  problem = Problem.new ChapterNumber.new(params[:chapter]), params[:number]

  content_type 'image/svg+xml'
  Graph.render_svg problem.graph_path
end

get '/:chapter/problems/:number.?:format?' do
  catalog = Catalog.new SOLUTION_ROOT
  problem = Problem.new ChapterNumber.new(params[:chapter]), params[:number]

  Renderer.render_problem problem, catalog
end

get '/:chapter/:section/:number.png' do
  exercise = Exercise.new ChapterNumber.new(params[:chapter]), params[:section], params[:number]

  content_type 'image/png'
  Graph.render_png exercise.graph_path
end

get '/:chapter/:section/:number.drawing.:index.svg' do
  exercise = Exercise.new ChapterNumber.new(params[:chapter]), params[:section], params[:number]

  content_type 'image/svg+xml'
  Graph.render_drawing exercise.draw_path, params[:index]
end

get '/:chapter/:section/:number.svg' do
  exercise = Exercise.new ChapterNumber.new(params[:chapter]), params[:section], params[:number]

  content_type 'image/svg+xml'
  Graph.render_svg exercise.graph_path
end


get '/:chapter/:section/:number.?:format?' do
  catalog = Catalog.new SOLUTION_ROOT
  exercise = Exercise.new ChapterNumber.new(params[:chapter]), params[:section], params[:number]

  Renderer.render_exercise exercise, catalog
end

get '/css/clrs.css' do
  content_type 'text/css'
  Renderer.render_css
end
