require 'rake/clean'
require_relative 'rakelib/ascii_to_video_utils'
require_relative 'rakelib/dodgeball_runner'
require 'stringio'

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
                      .exclude(/README\.md/)
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
CLEAN.include("**/*.fas")
CLEAN.include(MP_SCRIPT)
SP_READY_AGENTS = [
                   :brazdma1,#late
                   :cackolen,
                   :chmelond,
                   :cincuada,
                   :cadekva1,#late
                   :fifiksta,#clisp only
                   :fiserale,#sometimes fails
                   :hanafran,
                   :hlusiond,
                   :hrubaeli,
                   :kacurtom,
                   #:kersnmar,#not working
                   :kokorigo,
                   :kotrbluk,
                   :kroupvla,#late
                   :ludacrad,
                   :macalkar,
                   :milikjan,
                   :musilon4,#clisp only
                   :nohavja1,
                   :palkoigo,
                   :perutond,
                   :sembejir,
                   :silhaja6,
                   :staryvac,
                   :steklmar,
                   :stiplsta,
                   :strnaj11,
                   :temnymar,
                   :valespe3,
                   :vanikjak,
                   :wancavil
]
MP_READY_AGENTS = [
                   :brazdma1,#late
                   #:cackolen,#nil is not of type number
                   #:cadekva1,#late #polluting
                   :chmelond, #crashes sometimes
                   #:cincuada,#polluting
                   #:fifiksta,#clisp only #non symbol used
                   #:fiserale,#probably infilooped #ignore
                   #:hanafran,#polluting
                   :hlusiond,
                   :hrubaeli,
                   #:kacurtom,#polluting
                   #:kersnmar,#not working #polluting
                   :kokorigo,
                   #:kotrbluk,#nil is not of type number
                   #:kroupvla,#late #polluting
                   :ludacrad,
                   :macalkar,
                   #:milikjan,#nil is not of type number
                   #:musilon4,#clisp only #ignore
                   #:nohavja1,#nil is not of type number
                   :palkoigo,
                   :perutond,
                   :sembejir,
                   #:silhaja6,#polluting, #ignore
                   #:staryvac,#trying to take car of T
                   #:steklmar,#polluting, #ignore
                   #:stiplsta,#nil is not of type numberi #ignore
                   :strnaj11,
                   :temnymar,
                   :valespe3,
                   :vanikjak,
                   :wancavil
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
    
    IO.write(script_file, multi_player_test_script(agent))
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
    Dodgeball::DodgeballRunner.new("#{agent}/mp_script.lisp", "#{agent}/output").run
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

task :default => [:single_player, :multi_player]

desc "single player for each sp ready agent"
task :single_player => SP_SCRIPTS do
    SP_READY_AGENTS.each do |agent|
      puts "#{agent} started"
      ret_val = true
      with_captured_output do
         ret_val = Dodgeball::DodgeballRunner.new("#{agent}/sp_script.lisp", "#{agent}/output").run
      end

      if ret_val
        puts "#{agent} OK"
      else
        puts "#{agent} failed"
      end
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

def multi_player_test_script(agent)
  script = IO.read(SP_SCRIPT_TEMPLATE)
  script.gsub("<AGENT_PATH>", "#{Dir.pwd}/#{agent}/#{agent}_mp.lisp").gsub("<AGENT_NAME>", agent)
end

def single_player_script(agent) 
  script = IO.read(SP_SCRIPT_TEMPLATE)
  script.gsub("<AGENT_PATH>", "#{Dir.pwd}/#{agent}/#{agent}_sp.lisp").gsub("<AGENT_NAME>", agent)
end

def with_captured_output
  old_stdout = STDOUT.clone
  pipe_r, pipe_w = IO.pipe
  pipe_r.sync = true
  output = ""
  reader = Thread.new do
    begin
      loop do
        output << pipe_r.readpartial(1024)
      end
    rescue EOFError
    end
  end
  STDOUT.reopen(pipe_w)
  yield
ensure 
  STDOUT.reopen(old_stdout)
  pipe_w.close
  reader.join
  return output
end
