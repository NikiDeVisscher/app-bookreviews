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

  match "/books/*path", @any do
    IO.puts "INFO: Routing to books"
    Proxy.forward conn, path, "http://resource/books/"
  end

  match "/authors/*path", @any do
    IO.puts "INFO: Routing to authors"
    Proxy.forward conn, path, "http://resource/authors/"
  end

  match "/*_", %{ layer: :not_found } do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end
end
