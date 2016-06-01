defmodule ElixirTalk.Mixfile do
  use Mix.Project

  @version "1.1.0"

  def project do
    [ app: :elixir_talk,
      version: @version,
      elixir: "~> 1.0",
      description: description,
      package: package,
      deps: deps,

      name: "ElixirTalk",
      docs: [extras: ["README.md"],
             source_ref: "v#{@version}", main: "ElixirTalk",
             source_url: "https://github.com/jsvisa/elixir_talk"]
    ]
  end

  def application do
    [applications: [:logger, :yaml_elixir]]
  end

  defp deps do
    [{:earmark,     "~> 0.1", only: :doc},
     {:ex_doc,      "~> 0.11", only: :doc},
     {:yaml_elixir, "~> 1.0.0"},
     {:yamerl,      github: "yakaz/yamerl"}]
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
