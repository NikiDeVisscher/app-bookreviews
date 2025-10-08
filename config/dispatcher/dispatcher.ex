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

  get "/favicon.ico", @static do
    send_resp( conn, 404, "" )
  end

  match "/books/*path", @any do
    Proxy.forward conn, path, "http://resource/book/"
  end

  match "/persons/*path", @any do
    Proxy.forward conn, path, "http://resource/person/"
  end

  #match "/*_", %{ layer: :not_found } do
  #  send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  #end
end
