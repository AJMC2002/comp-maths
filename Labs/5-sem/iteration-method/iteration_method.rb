# frozen_string_literal: true

require 'numo/openblas'

module Numo
  class NArray
    # Uses the iteration method
    def eigenvalues(step = 0, v_cur = DFloat.ones([shape[0], 1]).fill(1))
      v_next = Linalg.matmul(self, v_cur) / Linalg.norm(Linalg.matmul(self, v_cur))
      if Linalg.norm(v_next - v_cur) < 1e-6 || step > 1e6
        (Linalg.matmul(Linalg.matmul(v_cur.transpose, self),
                       v_cur) / Linalg.matmul(v_cur.transpose, v_cur)).to_f
      else
        eigenvalues(step + 1, v_next)
      end
    end
  end
end

a = Numo::DFloat[[1, 2, 3], [2, 3, 4], [3, 4, 5]]
puts "Max. eigenvalue\n#{a.eigenvalues}"
puts
puts "All eigenvalues\n#{Numo::Linalg.eig(a).inspect}"
