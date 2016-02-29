defmodule ElixirTalk.Mixfile do
  use Mix.Project

  def project do
    [ app: :elixir_talk,
      version: "1.0.2",
      elixir: "~> 1.0.0",
      description: description,
      package: package,
      deps: deps ]
  end

  def application do
    []
  end

  defp deps do
    []
  end

  defp description do
    """
    ElixirTalk is an Elixir client for beanstalkd.
    """
  end

  defp package do
    [ files: ["lib", "test", "mix.exs", "README.md", "LICENSE"],
      contributors: ["Delweng Zheng"],
      licenses: ["MIT"],
      links: %{"GitHub": "https://github.com/jsvisa/elixir_talk"} ]
  end
end
