defmodule Env do ##################################################################################

  # TEST ------------------------------------------------------------------------------------------
  def test() do
    # [{:match, {:var, :x}, {:atm,:a}}, {:match, {:var, :y}, {:cons, {:var, :x}, {:atm, :b}}}, {:match, {:cons, :ignore, {:var, :z}}, {:var, :y}}, {:var, :z}]
    # [{:match, {:var, :x}, {:atm, :a}}, {:case, {:var, :x}, [{:clause, {:atm, :b}, [{:atm, :ops}]}, {:clause, {:atm, :a}, [{:atm, :yes}]} ]}]
    # [{:match, {:var, :x}, {:atm, :a}}, {:match, {:var, :f}, {:lambda, [:y], [:x], [{:cons, {:var, :x}, {:var, :y}}]}}, {:apply, {:var, :f}, [{:atm, :b}]}]
    # [{:match, {:var, :x}, {:cons, {:atm, :a}, {:cons, {:atm, :b}, {:atm, []}}}}, {:match, {:var, :y}, {:cons, {:atm, :c}, {:cons, {:atm, :d}, {:atm, []}}}}, {:apply, {:fun, :append}, [{:var, :x}, {:var, :y}]}]

    IO.puts("------------------------------------")
    m = new()
    IO.inspect(m)
    m = add(:x, :foo, m)
    IO.inspect(m)
    m = add(:y, :bar, m)
    IO.inspect(m)
    m = add(:x, :zoo, m)
    IO.inspect(m)
    m = remove([:x,:y], m)
    IO.inspect(m)
    IO.puts("------------------------------------")
  end

  # NEW -------------------------------------------------------------------------------------------
  def new() do [] end

  # ADD ------------------------------------------------------------------------------------------
  def add(key, str, env) do
    [{key, str} | env]
  end
  #def add(id, str, []) do [{id, str}] end  # [], :ok
  #def add(id, str, [{id, _} | t]) do [{id, str} | t] end  # k = k, :ok
  #def add(id, str, [{hp, tp}| t]) do [{hp, tp} | add(id, str, t)] end  # kx!=k, i

  # LOOKUP ----------------------------------------------------------------------------------------
  def lookup(key, env) do
    List.keyfind(env, key, 0)
  end
  #def lookup(_, []) do nil end  # [], :ok
  #def lookup(id, [{id, str}|_]) do {id, str} end  # id=id, :ok
  #def lookup(id, [{_, _}|t]) do lookup(id,t) end  # !id, i

  # REMOVE ----------------------------------------------------------------------------------------
  def remove(keys, env) do
    List.foldr(keys, env, fn(key, env) ->
      List.keydelete(env, key, 0)
    end)
  end
  #def remove([], _) do [] end  # Empty ids, :ok
  #def remove([h|t], env) do remove(t, remove(h, env)) end  # Remove ids
  #def remove(_, []) do [] end  # Empty env, :ok
  #def remove(id, [{id, _} | t]) do t end  # id=id, :ok
  #def remove(id, [{kx, str} | t]) do [{kx, str} | remove(id, t)] end  # kx!=id, i

  # CLOSURE ---------------------------------------------------------------------------------------
  def closure(keyss, env) do
    List.foldr(keyss, [], fn(key, acc) ->
      case acc do
        :error ->
          IO.puts("acc failed in closure")
          :error
        cls ->
          case lookup(key, env) do
            {key, value} ->
              [{key, value} | cls]
            nil ->
              IO.puts("lookup failed in closure")
              :error
          end
      end
    end)
  end

  # ARGUMENTS -------------------------------------------------------------------------------------
  def args(pars, args, env) do
    List.zip([pars, args]) ++ env
  end

end


defmodule Eager do ################################################################################

  # EVALUATE EXPRESSION ---------------------------------------------------------------------------

  def eval_expr({:atm, id}, _) do
    {:ok, id}
  end

  def eval_expr({:var, id}, env) do
    case Env.lookup(id, env) do
      nil ->
        IO.puts("Env.lookup failed in eval_expr for var")
        :error
      {_, str} -> {:ok, str}
    end
  end

  def eval_expr({:cons, e1, e2}, env) do
    case eval_expr(e1, env) do
      :error ->
        IO.puts("eval_expr 1 failed in eval_expr for cons")
        :error
      {:ok, s1} ->
        case eval_expr(e2, env) do
          :error ->
            IO.puts("eval_expr 2 failed in eval_expr for cons")
            :error
          {:ok, s2} -> {:ok, {s1, s2}}
        end
    end
  end

  def eval_expr({:case, expr, cls}, env) do
    case eval_expr(expr, env) do
      :error ->
        IO.puts("eval_expr failed in eval_expr for case")
        :error
      {:ok, str} -> eval_cls(cls, str, env)
    end
  end

  def eval_expr({:lambda, par, free, seq}, env) do
    case Env.closure(free, env) do
      :error ->
        IO.puts("Env.closure failed in eval_expr for lambda")
        :error
      closure -> {:ok, {:closure, par, seq, closure}}
    end
  end

  def eval_expr({:apply, expr, args}, env) do
    case eval_expr(expr, env) do
      :error ->
        IO.puts("eval_expr failed in eval_expr for apply")
        :error
      {:ok, {:closure, par, seq, closure}} ->
        case eval_args(args, env) do
        :error ->
          IO.puts("eval_args failed in eval_expr for apply")
          :error
        {:ok, strs} ->
          env = Env.args(par, strs, closure)
          eval_seq(seq, env)
        end
    end
  end

  def eval_expr({:fun, id}, _) do
    {par, seq} = apply(Prgm, id, [])
    {:ok, {:closure, par, seq, []}}
  end

  def eval_expr(_, _) do
    IO.puts("eval_expr failed, no matching expression found")
    :error
  end

  # EVALUATE MATCH --------------------------------------------------------------------------------

  def eval_match(:ignore, _, env) do
    {:ok, env}
  end

  def eval_match({:atm, id}, id, env) do
    {:ok, env}
  end

  def eval_match({:var, id}, str, env) do
    case Env.lookup(id, env) do
      nil -> {:ok, Env.add(id, str, env)}
      {_, ^str} -> {:ok, env}
      {_, _} ->
        IO.puts("Env.lookup failed in eval_match for var")
        :fail
    end
  end

  def eval_match({:cons, hp, tp}, {hs, ts}, env) do
    case eval_match(hp, hs, env) do
      :fail ->
        IO.puts("eval_match failed in eval_match for cons")
        :fail
      {:ok, new_env} -> eval_match(tp, ts, new_env)
    end
  end

  def eval_match(_, _, _) do
    #IO.puts("eval match failed")
    :fail
  end

  # EVALUATE SEQUENCE --------------------------------------------------------------------------------------

  def extract_vars({:var, v}) do [v] end
  def extract_vars({:cons, hp, tp}) do extract_vars(hp) ++ extract_vars(tp) end
  def extract_vars(_) do [] end

  def eval(seq) do
    eval_seq(seq, [])
  end

  def eval_scope(ptr, env) do
    Env.remove(extract_vars(ptr), env)
  end

  def eval_seq([exp], env) do
    eval_expr(exp, env)
  end

  def eval_seq([{:match, pat, exp} | seq], env) do
    case eval_expr(exp, env) do
      :error ->
        IO.puts("eval_expr failed in eva_seq for match")
        :error
      {:ok, str} ->
        scope = eval_scope(pat, env)
        case eval_match(pat, str, scope) do
          :fail ->
            IO.puts("eval_match failed in eval_seq for match")
            :error
          {:ok, new_env} -> eval_seq(seq, new_env)
        end
    end
  end

  # EVALUATE CLAUSE -------------------------------------------------------------------------------

  def eval_cls([], _, _) do
    IO.puts("No more clauses")
    :error
 end

 def eval_cls([{:clause, ptr, seq} | cls], str, env) do
   case eval_match(ptr, str, eval_scope(ptr, env)) do
     :fail -> eval_cls(cls, str, env)
     {:ok, new_env} -> eval_seq(seq, new_env)
   end
 end

 # EVALUATE ARGUMENTS -----------------------------------------------------------------------------

 def eval_args(args, env) do
   eval_args(args, env, [])
 end

 def eval_args([], _, strs) do {:ok, Enum.reverse(strs)} end
 def eval_args([expr | exprs], env, strs) do
  case eval_expr(expr, env) do
    :error ->
      IO.puts("eval_expr failed in eval_args")
      :error
    {:ok, str} -> eval_args(exprs, env, [str | strs])
  end
 end

end

defmodule Prgm do #################################################################################
  def append() do
    {[:x, :y],
      [{:case, {:var, :x},
      [{:clause, {:atm, []}, [{:var, :y}]},
        {:clause, {:cons, {:var, :hd}, {:var, :tl}},
          [{:cons,
            {:var, :hd},
            {:apply, {:fun, :append}, [{:var, :tl}, {:var, :y}]}}]
        }]
      }]
    }
  end
end
