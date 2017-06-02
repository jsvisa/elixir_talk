defmodule ElixirTalk.Protocol do
  require Logger

  @spec parse(binary) :: {:ok, term, binary} | :more
  def parse(<<"OUT_OF_MEMORY\r\n", rest :: binary>>),   do: {:ok, :out_of_memory, rest}
  def parse(<<"INTERNAL_ERROR\r\n", rest :: binary>>),  do: {:ok, :internal_error, rest}
  def parse(<<"DRAINING\r\n", rest :: binary>>),        do: {:ok, :draining, rest}
  def parse(<<"BAD_FORMAT\r\n", rest :: binary>>),      do: {:ok, :bad_format, rest}
  def parse(<<"UNKNOWN_COMMAND\r\n", rest :: binary>>), do: {:ok, :unknown_command, rest}
  def parse(<<"EXPECTED_CRLF\r\n", rest :: binary>>),   do: {:ok, :expected_crlf, rest}
  def parse(<<"JOB_TOO_BIG\r\n", rest :: binary>>),     do: {:ok, :job_too_big, rest}
  def parse(<<"DEADLINE_SOON\r\n", rest :: binary>>),   do: {:ok, :deadline_soon, rest}
  def parse(<<"TIMED_OUT\r\n", rest :: binary>>),       do: {:ok, :timed_out, rest}
  def parse(<<"DELETED\r\n", rest :: binary>>),         do: {:ok, :deleted, rest}
  def parse(<<"NOT_FOUND\r\n", rest :: binary>>),       do: {:ok, :not_found, rest}
  def parse(<<"RELEASED\r\n", rest :: binary>>),        do: {:ok, :released, rest}
  def parse(<<"BURIED\r\n", rest :: binary>>),          do: {:ok, :buried, rest}
  def parse(<<"TOUCHED\r\n", rest :: binary>>),         do: {:ok, :touched, rest}
  def parse(<<"NOT_IGNORED\r\n", rest :: binary>>),     do: {:ok, :not_ignored, rest}
  def parse(<<"KICKED\r\n", rest :: binary>>),          do: {:ok, :kicked, rest}
  def parse(<<"INSERTED ", bin :: binary>>),            do: parse_int(bin, :inserted)
  def parse(<<"BURIED ", bin :: binary>>),              do: parse_int(bin, :buried)
  def parse(<<"WATCHING ", bin :: binary>>),            do: parse_int(bin, :watching)
  def parse(<<"KICKED ", bin :: binary>>),              do: parse_int(bin, :kicked)
  def parse(<<"USING ", bin :: binary>>),               do: parse_str(bin, :using)
  def parse(<<"RESERVED ", bin :: binary>>),            do: parse_job(bin, :reserved)
  def parse(<<"FOUND ", bin :: binary>>),               do: parse_job(bin, :found)
  def parse(<<"OK ", bin :: binary>>),                  do: parse_yml(bin)
  def parse(_),                                         do: :more

  defp parse_int(bin, name) do
    case parse_digits(bin) do
      {:ok, int, <<"\r\n", rest :: binary>>} ->
        {:ok, {name, int}, rest}
      _ ->
        :more
    end
  end

  def parse_digits(bin), do: parse_digits(bin, 0)

  def parse_digits(<<d :: integer, rest :: binary>>, acc) when d >= ?0 and d <= ?9,
    do: parse_digits(rest, acc * 10 + d - ?0)
  def parse_digits(rest, acc),
    do: {:ok, acc, rest}

  defp parse_str(bin, name) do
    {:ok, str, rest} = parse_chars(bin, "")
    {:ok, {name, str}, rest}
  end

  defp parse_chars(<<"\r\n", rest :: binary>>, acc),
    do: {:ok, acc, rest}
  defp parse_chars(<<c :: utf8, rest :: binary>>, acc),
    do: parse_chars(rest, <<acc :: binary, c :: utf8>>)

  defp parse_job(bin, name) do
    case parse_id(bin) do
      {:ok, id, bin} ->
        case parse_body(bin) do
          {:ok, body, rest} ->
            {:ok, {name, id, body}, rest}
          :more ->
            :more
        end
      :more ->
        :more
    end
  end

  defp parse_id(bin) do
    case parse_digits(bin) do
      {:ok, id, <<" ", rest :: binary>>} ->
        {:ok, id, rest}
      _ ->
        :more
    end
  end

  defp parse_body(bin) do
    # Logger.debug "Recv bytes: #{inspect bin}"
    case parse_digits(bin) do
      {:ok, length, <<"\r\n", rest :: binary>>} ->
        case rest do
          <<body :: size(length)-binary, "\r\n", rest :: binary>> ->
            {:ok, body, rest}
          _ ->
            :more
        end
      _ ->
        :more
    end
  end

  defp parse_yml(bin) do
    case parse_body(bin) do
      {:ok, body, rest} ->
        {:ok, YamlElixir.read_from_string(body), rest}
      _ ->
        :more
    end
  end

end

