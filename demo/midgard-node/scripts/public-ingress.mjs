/**
 * The optional TLS ingress in front of a preprod-public node. The full-stack
 * command (`e2e-stack`) renders it into its run directory and runs it on the
 * node's isolated ingress network; the node trusts only the ingress address
 * to name clients (HTTP_TRUSTED_PROXY_PEERS).
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
/**
 * Health, readiness and admin routes get a per-IP zone of their own and no
 * global zone, so a public flood that drains the global zone cannot refuse
 * them; a location's own limit_req replaces the server-level ones.
 */
export const renderPublicIngress = (env) => {
  const hostname = publicApiHostname(env);
  const port = env.PORT ?? "3000";
  if (!/^[1-9][0-9]{0,4}$/u.test(port))
    throw new Error("E_PUBLIC_INGRESS_PORT");
  // The stack's node service, on the ingress network.
  const upstream = `https://midgard-node:${port}`;
  return `pid /tmp/nginx.pid;
error_log /dev/null;
events { worker_connections 512; }
http {
  server_tokens off;
  client_header_buffer_size 1k;
  large_client_header_buffers 2 4k;
  client_header_timeout 10s;
  client_body_timeout 10s;
  send_timeout 10s;
  keepalive_timeout 5s;
  keepalive_requests 100;
  client_body_temp_path /tmp/client-body;
  proxy_temp_path /tmp/proxy;
  fastcgi_temp_path /tmp/fastcgi;
  uwsgi_temp_path /tmp/uwsgi;
  scgi_temp_path /tmp/scgi;
  map $request_method $log_method { default other; GET GET; POST POST; OPTIONS OPTIONS; }
  map $uri $log_route { default unknown; /healthz healthz; /readyz readyz; /submit submit; /tx/evaluate evaluate; /tx tx; /tx-status tx-status; /utxos utxos; /txs txs; /block block; /init init; /commit commit; /merge merge; }
  log_format public escape=json '{"method":"$log_method","route":"$log_route","status":$status,"bytes":$body_bytes_sent,"seconds":$request_time}';
  access_log /dev/stdout public;
  limit_req_zone $binary_remote_addr zone=per_ip:1m rate=10r/s;
  limit_req_zone $server_name zone=global:64k rate=100r/s;
  limit_req_zone $binary_remote_addr zone=liveness_ip:1m rate=10r/s;
  limit_conn_zone $binary_remote_addr zone=connections:1m;
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
    limit_req zone=per_ip burst=30 nodelay;
    limit_req zone=global burst=200 nodelay;
    limit_req_status 429;
    add_header Retry-After 5 always;
    add_header Strict-Transport-Security "max-age=31536000" always;
    proxy_pass_request_headers off;
    proxy_set_header Host ${hostname};
    proxy_set_header Content-Type $http_content_type;
    proxy_set_header Authorization $http_authorization;
    proxy_set_header X-Midgard-Admin-Key $http_x_midgard_admin_key;
    proxy_set_header X-Midgard-Client-IP $remote_addr;
    proxy_set_header X-Midgard-Admin-Session $http_x_midgard_admin_session;
    proxy_set_header X-Midgard-Admin-Timestamp $http_x_midgard_admin_timestamp;
    proxy_set_header X-Midgard-Admin-Nonce $http_x_midgard_admin_nonce;
    proxy_set_header X-Midgard-Admin-Signature $http_x_midgard_admin_signature;
    proxy_http_version 1.1;
    proxy_request_buffering off;
    proxy_buffering off;
    proxy_ignore_client_abort off;
    proxy_next_upstream off;
    proxy_connect_timeout 3s;
    proxy_read_timeout 30s;
    proxy_send_timeout 10s;
    proxy_ssl_verify on;
    proxy_ssl_trusted_certificate /run/public/tls-ca.pem;
    proxy_ssl_name ${hostname};
    proxy_ssl_server_name on;
    proxy_ssl_protocols TLSv1.3;
    location = /submit { client_max_body_size 64m; proxy_pass ${upstream}; }
    location = /tx/evaluate { client_max_body_size 64m; proxy_pass ${upstream}; }
    location ~ ^/(healthz|readyz|init|commit|merge|stateQueue|logBlocksDB|logGlobals|operator/status|admin/session)$ { limit_req zone=liveness_ip burst=30 nodelay; proxy_pass ${upstream}; }
    location / { proxy_pass ${upstream}; }
  }
}
`;
};
