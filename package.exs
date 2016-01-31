defmodule erlchronos.Mixfile do
  use Mix.Project

  @version File.read!("VERSION") |> String.strip

  def project do
    [app: :erlchronos,
     version: @version,
     description: "Erlang/OTP gen_server wrapper with ticks",
     package: package]
  end

  defp package do
    [files: ~w(src rebar.config README.md LICENSE),
     contributors: ["Guilherme Andrade"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/g-andrade/erlchronos"}]
  end
end
