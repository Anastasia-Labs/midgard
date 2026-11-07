/**
 * The TLS ingress in front of a preprod-public node. The full-stack command
 * (`e2e-stack`) renders it into its run directory and runs it beside the
 * node. TLS ends here; nginx forwards plain HTTP to the node's Unix socket
 * (HTTP_SOCKET_PATH), and the node takes the client address from the
 * X-Midgard-Client-IP header only on that socket.
 */
export const publicApiHostname = (env) => {
  const value = env.MIDGARD_PUBLIC_API_ORIGIN;
  let origin;
  try {
    origin = new URL(value);
  } catch {
    throw new Error("E_PUBLIC_INGRESS_ORIGIN");
  }
  if (
    origin.protocol !== "https:" ||
    origin.port !== "" ||
    origin.username !== "" ||
    origin.password !== "" ||
    origin.pathname !== "/" ||
    origin.search !== "" ||
    origin.hash !== "" ||
    !/^(?:[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?$/u.test(
      origin.hostname,
    )
  )
    throw new Error("E_PUBLIC_INGRESS_ORIGIN");
  return origin.hostname;
};
const UNIX_SOCKET_PATH = /^\/[A-Za-z0-9._/-]{1,99}$/u;

/**
 * Renders the nginx configuration. `socketPath` is the node's socket as
 * mounted in the ingress container; `submitBodyBytes` is the node's largest
 * `/submit` and `/tx/evaluate` body (one submission envelope).
 *
 * The node's own limiter sets the request rates, so nginx refuses no submit
 * or read request for rate. nginx keeps a cap of 4 concurrent requests per
 * address (the node's own per-client cap) and a per-address rate on health,
 * readiness and admin routes, which no public flood can drain. The refusals
 * nginx makes itself answer in the node's JSON shape with Retry-After;
 * responses from the node pass through untouched.
 */
export const renderPublicIngress = (env, { socketPath, submitBodyBytes }) => {
  const hostname = publicApiHostname(env);
  if (
    typeof socketPath !== "string" ||
    !UNIX_SOCKET_PATH.test(socketPath) ||
    socketPath.split("/").includes("..")
  )
    throw new Error("E_PUBLIC_INGRESS_SOCKET");
  if (!Number.isSafeInteger(submitBodyBytes) || submitBodyBytes <= 0)
    throw new Error("E_PUBLIC_INGRESS_BODY");
  const busy = JSON.stringify({
    error: "HTTP ingress capacity is full",
    errorCode: "E_HTTP_INGRESS_CAPACITY",
  });
  const unavailable = JSON.stringify({
    error: "The node is unavailable; retry later",
    errorCode: "E_HTTP_INGRESS_CAPACITY",
  });
  const limited = JSON.stringify({
    error: "Request rate exceeded",
    errorCode: "E_HTTP_RATE_LIMITED",
  });
  return `pid /tmp/nginx.pid;
error_log /dev/null;
worker_processes 4;
events { worker_connections 4096; }
http {
  server_tokens off;
  client_header_buffer_size 1k;
  large_client_header_buffers 2 4k;
  client_header_timeout 10s;
  client_body_timeout 10s;
  send_timeout 10s;
  keepalive_timeout 5s;
  keepalive_requests 1000;
  client_body_temp_path /tmp/client-body;
  proxy_temp_path /tmp/proxy;
  fastcgi_temp_path /tmp/fastcgi;
  uwsgi_temp_path /tmp/uwsgi;
  scgi_temp_path /tmp/scgi;
  map $request_method $log_method { default other; GET GET; POST POST; OPTIONS OPTIONS; }
  map $uri $log_route { default unknown; /healthz healthz; /readyz readyz; /submit submit; /tx/evaluate evaluate; /tx tx; /tx-status tx-status; /utxos utxos; /txs txs; /block block; /init init; /commit commit; /merge merge; /peer/txs peer-txs; /peer/inventory peer-inventory; }
  log_format public escape=json '{"method":"$log_method","route":"$log_route","status":$status,"bytes":$body_bytes_sent,"seconds":$request_time}';
  access_log /dev/stdout public;
  limit_req_zone $binary_remote_addr zone=liveness_ip:1m rate=10r/s;
  limit_conn_zone $binary_remote_addr zone=connections:1m;
  # Plain HTTP to the node over its socket. Idle connections stay open for
  # reuse; the node keeps them 65 s, longer than nginx does, so nginx closes
  # first and never sends on a connection the node is closing.
  upstream midgard_node {
    server unix:${socketPath};
    keepalive 64;
    keepalive_requests 10000;
    keepalive_timeout 60s;
  }
  server {
    listen 8443 ssl;
    server_name ${hostname};
    ssl_certificate /run/public/tls-cert.pem;
    ssl_certificate_key /run/secrets/tls-key.pem;
    ssl_protocols TLSv1.3;
    ssl_session_cache shared:TLS:1m;
    client_max_body_size 64k;
    limit_conn connections 4;
    limit_conn_status 503;
    limit_req_status 429;
    error_page 503 = @busy;
    error_page 429 = @limited;
    error_page 502 504 = @unavailable;
    add_header Strict-Transport-Security "max-age=31536000" always;
    proxy_pass_request_headers off;
    proxy_set_header Host ${hostname};
    proxy_set_header Connection "";
    proxy_set_header Content-Type $http_content_type;
    proxy_set_header Authorization $http_authorization;
    proxy_set_header X-Midgard-Admin-Key $http_x_midgard_admin_key;
    proxy_set_header X-Midgard-Client-IP $remote_addr;
    proxy_set_header X-Midgard-Admin-Session $http_x_midgard_admin_session;
    proxy_set_header X-Midgard-Admin-Timestamp $http_x_midgard_admin_timestamp;
    proxy_set_header X-Midgard-Admin-Nonce $http_x_midgard_admin_nonce;
    proxy_set_header X-Midgard-Admin-Signature $http_x_midgard_admin_signature;
    proxy_set_header X-Midgard-Operator-Key $http_x_midgard_operator_key;
    proxy_set_header X-Midgard-Operator-Signature $http_x_midgard_operator_signature;
    proxy_http_version 1.1;
    proxy_request_buffering off;
    proxy_buffering off;
    proxy_ignore_client_abort off;
    proxy_next_upstream off;
    proxy_connect_timeout 3s;
    proxy_read_timeout 30s;
    proxy_send_timeout 10s;
    location = /submit { client_max_body_size ${submitBodyBytes.toString()}; proxy_pass http://midgard_node; }
    location = /tx/evaluate { client_max_body_size ${submitBodyBytes.toString()}; proxy_pass http://midgard_node; }
    location = /peer/txs { client_max_body_size 1m; proxy_pass http://midgard_node; }
    location = /peer/inventory { client_max_body_size 1m; proxy_pass http://midgard_node; }
    location ~ ^/(healthz|readyz|init|commit|merge|stateQueue|logBlocksDB|logGlobals|operator/status|admin/session)$ { limit_req zone=liveness_ip burst=30 nodelay; proxy_pass http://midgard_node; }
    location / { proxy_pass http://midgard_node; }
    location @busy { default_type application/json; add_header Retry-After 5 always; return 503 '${busy}'; }
    location @limited { default_type application/json; add_header Retry-After 1 always; return 429 '${limited}'; }
    location @unavailable { default_type application/json; add_header Retry-After 5 always; return 503 '${unavailable}'; }
  }
}
`;
};
