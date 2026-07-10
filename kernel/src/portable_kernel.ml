module Atomic = struct
  include Atomic
  module Array = Atomic_array
end

module Atomic_array = Atomic_array
module Capsule = Capsule
module Shards = Shards
module Subatomic = Subatomic
