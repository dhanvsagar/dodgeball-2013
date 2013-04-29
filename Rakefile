require 'pry'
require 'rake/clean'
require_relative 'rakelib/ascii_to_video_utils'
require_relative 'rakelib/dodgeball_runner'

SP_SCRIPT_TEMPLATE = "ENVIRONMENT/aimaload.lisp"
MP_SCRIPT_TEMPLATE = "ENVIRONMENT/aimaload_mp.lisp"
MP_SCRIPT = "mp_script.lisp"
AGENTS = FileList["*"].exclude(/ENVIRONMENT.*/)
                      .exclude(/sublime/)
                      .exclude(/Rakefile/)
                      .exclude(/output/)
                      .exclude(/mp_script\.lisp/)
                      .exclude(/tags/)
                      .exclude(/images/)
                      .exclude(/result\.mp4/)
SP_SCRIPTS = AGENTS.collect { |a| "#{a}/sp_script.lisp" }
MP_AGENTS = AGENTS.collect { |a| "#{a}/#{a}_mp.lisp" }
CLEAN.include("*/sp_script.lisp")
CLEAN.include("*/mp_script.lisp")
CLEAN.include("images")
CLEAN.include("*/images")
CLEAN.include("output")
CLEAN.include("*/output")
CLEAN.include("*/*.gif")
CLEAN.include("result.mp4")
CLEAN.include("*/*.mp4")
CLEAN.include("**/*.fasl")
CLEAN.include(MP_SCRIPT)
SP_READY_AGENTS = [:cackolen,
                   :chmelond,
                   :cincuada,
                   #:fifiksta,
                   :fiserale,
                   :hanafran,
                   :hlusiond,
                   :hrubaeli,
                   :kacurtom,
                   #:kersnmar,
                   :kokorigo,
                   #:kotrbluk,
                   #:ludacrad,
                   :macalkar,
                   #:musilon4,
                   :nohavja1,
                   #:palkoigo,
                   :perutond,
                   #:sembejir,
                   :silhaja6,
                   #:staryvac,
                   :steklmar,
                   :stiplsta,
                   :strnaj11,
                   :temnymar,
                   :valespe3,
                   :vanikjak
]
MP_READY_AGENTS = [:cackolen,
                   :chmelond,
                   #:cincuada,
                   #:fifiksta,
                   #:fiserale,
                   #:hanafran,
                   :hlusiond,
                   #:hrubaeli,
                   :kacurtom,
                   #:kersnmar,
                   :kokorigo,
                   #:kotrbluk,
                   #:ludacrad,
                   #:macalkar,
                   #:musilon4,
                   #:nohavja1,
                   #:palkoigo,
                   #:perutond,
                   #:sembejir,
                   #:silhaja6,
                   #:staryvac,
                   #:steklmar,
                   #:stiplsta,
                   #:strnaj11,
                   :temnymar,
                   #:valespe3,
                   :vanikjak
]

AGENTS.each do |agent|
  file "#{agent}/sp_script.lisp" => "#{agent}/#{agent}_sp.lisp" do
    script_file = "#{agent}/sp_script.lisp"
    
    File.open script_file, 'w+' do |f|
      f.puts single_player_script(agent)
    end

    File.chmod 0755, script_file
  end

  file "#{agent}/mp_script.lisp" => "#{agent}/#{agent}_mp.lisp" do
    script_file = "#{agent}/mp_script.lisp"
    
    File.open script_file, 'w+' do |f|
      f.puts single_player_script(agent)
    end

    File.chmod 0755, script_file
  end

  file "#{agent}/#{agent}_sp.lisp" 
  file "#{agent}/#{agent}_mp.lisp" 

  desc "single player with agent #{agent}"
  task "#{agent}_sp" => "#{agent}/sp_script.lisp" do
    Dodgeball::DodgeballRunner.new("#{agent}/sp_script.lisp", "#{agent}/output").run
  end

  desc "single player with multi player agent #{agent}"
  task "#{agent}_mp" => "#{agent}/mp_script.lisp" do
    Dodgeball::DodgeballRunner.new("#{agent}/sp_script.lisp", "#{agent}/output").run
  end

  desc "generate animation for single player with agent #{agent}"
  task "#{agent}_sp_mp4" => "#{agent}_sp" do
    AsciiToVideoUtils.create_animation("#{agent}/output", 
                                       "#{agent}/images", 
                                       "#{agent}/#{agent}.mp4", 
                                        3)
  end

end

file MP_SCRIPT => MP_AGENTS do
    IO.write(MP_SCRIPT, multi_player_script)
    File.chmod 0755, MP_SCRIPT
end

task :default => [:single_player, :multiplayer]

desc "single player for each sp ready agent"
task :single_player => SP_SCRIPTS do
  SP_READY_AGENTS.each do |agent|
    Dodgeball::DodgeballRunner.new("#{agent}/sp_script.lisp", "#{agent}/output").run
  end
end

desc "single player for sp ready agent, with pause after each run"
task :single_player_debug => SP_SCRIPTS do
  SP_READY_AGENTS.each do |agent|
    Dodgeball::DodgeballRunner.new("#{agent}/sp_script.lisp", "#{agent}/output").run
    puts agent
    puts "Continue?"
    answer = STDIN.gets.chomp
    exit if answer != 'y' && answer != ''
  end
end

desc "multi player with all mp ready agents"
task :multi_player => MP_SCRIPT do
  Dodgeball::DodgeballRunner.new(MP_SCRIPT, "output").run
end

task :multi_player_mp4 => :multi_player do
  AsciiToVideoUtils.create_animation("output", "images", "result.mp4")
end

def multi_player_script
  script = IO.read(MP_SCRIPT_TEMPLATE)
  script
    .gsub("<AGENT_PATHS>", MP_READY_AGENTS.collect {|a| "(print \"Loading #{a}\")\n(load \"#{a}/#{a}_mp.lisp\")\n(print \"#{a} sucessfully loaded\")" }.join("\n"))
    .gsub("<AGENT_NAMES>", MP_READY_AGENTS.collect {|a| "'#{a}" }.join(" "))
end

def single_player_script(agent) 
  script = IO.read(SP_SCRIPT_TEMPLATE)
  script.gsub("<AGENT_PATH>", "#{Dir.pwd}/#{agent}/#{agent}_sp.lisp").gsub("<AGENT_NAME>", agent)
end
