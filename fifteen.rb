module Advent
  class NumberGame
    def speaker(input)
      Enumerator.new do |e|
        speaking_times_by_number = {}
        utterances = []
        now = 0

        input.each.with_index do |number, index|
          speaking_times_by_number[number] ||= []
          speaking_times_by_number[number] << index

          utterances << number
          now += 1
        end

        utterances.each { |number| e << number }

        loop do
          last_spoken_number = utterances.last
          times_spoken = speaking_times_by_number[last_spoken_number]

          if times_spoken.length == 1
            number_to_speak = 0
          else
            last_last_time, last_time = times_spoken.last(2)
            number_to_speak = last_time - last_last_time
            times_spoken.shift
          end

          e << number_to_speak

          speaking_times_by_number[number_to_speak] ||= []
          speaking_times_by_number[number_to_speak] << now
          utterances << number_to_speak
          now += 1
        end
      end
    end
  end
end
