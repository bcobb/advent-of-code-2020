class Node
  def initialize(value, next_node = nil)
    @value = value
    @next_node = next_node || self
  end

  # before: me -> next
  # then: given node
  # after: me -> node -> next
  def insert(node)
    node.next_node = @next_node
    @next_node = node
  end

  def insert_segment(segment_start, segment_end)
    segment_end.next_node = @next_node
    @next_node = segment_start
  end

  def skip_next(n)
    n.times do
      @next_node = next_node.next_node
    end
  end

  attr_accessor :next_node
  attr_reader :value
end

class Game
  def initialize(labels, additional_cups = [])
    @labels = labels.chars.map(&:to_i) + additional_cups
    @max_label = @labels.max
    @cups_by_label = @labels.reduce([{}, nil]) do |(index, previous), label|
      node = Node.new(label)

      if previous
        previous.insert(node)
      end

      index[label] = node

      [index, node]
    end.first
    @current = @cups_by_label[@labels.first]
  end

  def move
    first_pickup = @current.next_node
    second_pickup = first_pickup.next_node
    third_pickup = second_pickup.next_node

    unavailable_destinations = [first_pickup.value, second_pickup.value, third_pickup.value]
    proposed_destination = @current.value.pred

    destination_label = loop do
      if unavailable_destinations.include?(proposed_destination)
        proposed_destination -= 1
      elsif proposed_destination < 1
        proposed_destination = @max_label
      else
        break proposed_destination
      end
    end

    destination_cup = @cups_by_label[destination_label]

    @current.skip_next(3)
    destination_cup.insert_segment(first_pickup, third_pickup)
    @current = @current.next_node
  end

  attr_reader :cups_by_label

  def each(starting_label: 1)
    Enumerator.new do |e|
      node = @cups_by_label[starting_label]
      e << node

      loop do
        node = node.next_node
        e << node
      end
    end
  end

  def inspect
    "#<Game current=#{@current.value}>"
  end
end

# part one
# game = Game.new("685974213")
# 100.times { game.move }
# game.each.take(9).drop(1).map(&:value).join

# part two
# game = Game.new("685974213", (10..1_000_000).to_a)
# 10_000_000.times { game.move }
# game.each.take(3).map(&:value).reduce(&:*)
