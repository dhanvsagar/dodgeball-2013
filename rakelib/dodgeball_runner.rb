require 'open3'

module Dodgeball
  class DodgeballRunner

    def initialize(script_path, output_dir)
      @script_path = script_path
      @output_dir = output_dir
    end

    def run
      Dir.mkdir(@output_dir) unless Dir.exist?(@output_dir)
      raise "No such script file #{@script_path}" unless File.exist?(@script_path)

      run_game_script
    end

    private
    
    def run_game_script
      reading_board = false
      board = ""
      counter = 1

      output = ""
      Open3.popen3("clisp #{@script_path} 2>&1") do |stdin, stdout, stderr, wait_thr|
      #Open3.popen3("alisp '-#D' #{@script_path} 2>&1") do |stdin, stdout, stderr, wait_thr|
        while line = stdout.gets
          puts line
          output << line
        end
      end

      output.each_line do |line|
        if line.match(/^[ ]*([0-9]| )+$/)
          board << line
          File.open("#{@output_dir}/%03d.txt" % counter, "w+") { |f| f.write(board) }
          board = ""
          counter = counter + 1
          reading_board = false
        elsif reading_board 
          board << line  
        elsif line.match(/^[ ]*[|-]+[ |-]*$/) 
          reading_board = true
          board << line
        end
      end
    end
  end
end
