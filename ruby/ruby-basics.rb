##
# Implement any sorting algorithm.
# Bonus: keep it concise
#


def sort(arr)
	arr.reduce([]) do |sorted, element|
		index = sorted.find_index do |item|
			element< item
		end
		index ||= sorted.length
		sorted.insert(index, element) 
	end
end


##
# Longest Collatz sequence
#
def euler14(n)
	cache = {1 => 1}
	2.upto(n) do |1|
		collatz(n,cache)
	end
	cache.values.max
end


def collatz(n, cache)
	#len = Hash.new
	unless cache.include? n
	counter  =1
	current = n
	while current != 1 do
		if cache.include? current
			counter += cahce[current]
			break
		end
		if current.even?
			current = current /2
		else
			current = current *3 +1
		end
		counter +=1
	end 
	cache[n] = counter
end
	cache[n]
end

##
# Return a random permutation of the elements.
#
def shuffle(arr)
	size = arr.length
	while sie >= 0 do
		swap = Random.rand(size)
		last = size -1
		tmp = arr[last]
		arr[last] = arr[swap]
		arr[swap] = tmp
		size -= 1
end
