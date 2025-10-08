defmodule Dispatcher do
  use Matcher
  define_accept_types [
    html: [ "text/html", "application/xhtml+html" ],
    json: [ "application/json", "application/vnd.api+json" ]
  ]

  @any %{}
  @json %{ accept: %{ json: true } }
  @html %{ accept: %{ html: true } }

  define_layers [ :static, :services, :fall_back, :not_found ]

  options "/*_path", @any do
    conn
    |> Plug.Conn.put_resp_header( "access-control-allow-headers", "content-type,accept" )
    |> Plug.Conn.put_resp_header( "access-control-allow-methods", "*" )
    |> send_resp( 200, "{ \"message\": \"ok\" }" )
  end

  match "/books/*path", @any do
    IO.puts "INFO: Routing to books"
    Proxy.forward conn, path, "http://resource/book/"
  end

  match "/authors/*path", @any do
    IO.puts "INFO: Routing to authors"
    Proxy.forward conn, path, "http://resource/author/"
  end

  match "/book/*path", @any do
    IO.puts "INFO: Routing to book"
    Proxy.forward conn, path, "http://resource/book/"
  end

  match "/author/*path", @any do
    IO.puts "INFO: Routing to author"
    Proxy.forward conn, path, "http://resource/author/"
  end

  match "/*_", %{ layer: :not_found } do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end
end
