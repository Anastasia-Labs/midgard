import { createHash } from "node:crypto";
import { isAbsolute } from "node:path";
import { fileURLToPath } from "node:url";
import {
  constants,
  closeSync,
  fstatSync,
  openSync,
  readSync,
  writeFileSync,
} from "node:fs";

const image = (env, name) => {
  const value = env[name];
  if (
    typeof value !== "string" ||
    !/^[a-z0-9][a-z0-9./:_-]*@sha256:[0-9a-f]{64}$/u.test(value)
  )
    throw new Error(`E_PUBLIC_COMPOSE_IMAGE: ${name}`);
  return value;
};
const mounted = (env, name) => {
  const value = env[name];
  if (typeof value !== "string" || !isAbsolute(value) || value.includes("\n"))
    throw new Error(`E_PUBLIC_COMPOSE_MOUNT: ${name}`);
  return value;
};
export const publicApiHostname = (env) => {
  const value = env.MIDGARD_PUBLIC_API_ORIGIN;
  let origin;
  try {
    origin = new URL(value);
  } catch {
    throw new Error("E_PUBLIC_COMPOSE_ORIGIN");
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
    throw new Error("E_PUBLIC_COMPOSE_ORIGIN");
  return origin.hostname;
};
const readPublicNodeConfig = (env) => {
  const path = mounted(env, "MIDGARD_CONFIG_FILE");
  let fd;
  try {
    fd = openSync(path, constants.O_RDONLY | constants.O_NOFOLLOW);
    const stat = fstatSync(fd);
    if (
      !stat.isFile() ||
      stat.size < 1 ||
      stat.size > 65_536 ||
      (stat.mode & 0o077) !== 0
    )
      throw new Error();
    const bytes = Buffer.alloc(65_537);
    let count = 0;
    while (count < bytes.length) {
      const read = readSync(fd, bytes, count, bytes.length - count, null);
      if (read === 0) break;
      count += read;
    }
    if (count === 0 || count > 65_536) throw new Error();
    const config = JSON.parse(bytes.subarray(0, count).toString("utf8"));
    const required = {
      PORT: "3000",
      POSTGRES_HOST: "postgres",
      POSTGRES_PORT: "5432",
      POSTGRES_USER: "midgard",
      POSTGRES_DB: "midgard",
      HTTP_TRUSTED_PROXY_PEERS: "10.91.7.2",
    };
    for (const [name, value] of Object.entries(required))
      if (config[name] !== value) throw new Error();
    return config;
  } catch {
    throw new Error(
      "E_PUBLIC_COMPOSE_CONFIG: mounted JSON configuration must match the isolated service topology and trusted proxy",
    );
  } finally {
    if (fd !== undefined) closeSync(fd);
  }
};
const bind = (source, target) => ({
  type: "bind",
  source,
  target,
  read_only: true,
  bind: { create_host_path: false },
});
const logs = {
  driver: "json-file",
  options: { "max-size": "10m", "max-file": "3" },
};
const restrictions = {
  read_only: true,
  cap_drop: ["ALL"],
  security_opt: ["no-new-privileges:true"],
  logging: logs,
  pids_limit: 512,
};

/** HTTP overlay for the existing full-stack authority, not a complete chain/DA deployment.
 * The full-stack owner must compose its authenticated producer manifest, private DA
 * transport, Cardano socket/config and socket group access before starting the node.
 */
export const renderPublicOperatorCompose = (env) => {
  const hostname = publicApiHostname(env);
  readPublicNodeConfig(env);
  if (!/^[0-9a-f]{40}$/u.test(env.MIDGARD_PUBLIC_RELEASE_REVISION ?? ""))
    throw new Error("E_PUBLIC_COMPOSE_REVISION");
  const roleImages = {
    node: image(env, "MIDGARD_PUBLIC_NODE_IMAGE"),
    postgres: image(env, "MIDGARD_PUBLIC_POSTGRES_IMAGE"),
    ingress: image(env, "MIDGARD_PUBLIC_INGRESS_IMAGE"),
  };
  const nodeEnv = {
    MIDGARD_DOTENV_MODE: "disabled",
    MIDGARD_CONFIG_FILE: "/run/secrets/node-config.json",
    MIDGARD_PUBLIC_API_ORIGIN: `https://${hostname}`,
    MIDGARD_PUBLIC_RELEASE_REVISION: env.MIDGARD_PUBLIC_RELEASE_REVISION,
    MIDGARD_PUBLIC_NODE_IMAGE: roleImages.node,
    MIDGARD_PUBLIC_POSTGRES_IMAGE: roleImages.postgres,
    MIDGARD_PUBLIC_INGRESS_IMAGE: roleImages.ingress,
    MIDGARD_PUBLIC_TLS_CERT_FILE: "/run/public/tls-cert.pem",
    MIDGARD_PUBLIC_TLS_KEY_FILE: "/run/secrets/tls-key.pem",
    MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: "/run/public/deployment-info.json",
    MIDGARD_PUBLIC_RELEASE_AUTHORITY_FILE: "/run/public/release-authority.json",
    MIDGARD_PUBLIC_RELEASE_PROVENANCE_FILE:
      "/run/public/release-provenance.json",
    MIDGARD_PUBLIC_RELEASE_TRUST_ROOTS_FILE:
      "/run/public/release-trust-roots.json",
    MIDGARD_PUBLIC_FINALIZED_MANIFEST_FILE:
      "/run/public/finalized-manifest.json",
    MIDGARD_PUBLIC_MANIFEST_ID: env.MIDGARD_PUBLIC_MANIFEST_ID,
    MIDGARD_DEPLOYMENT_PARAMETER_REGISTRY_FILE:
      "/run/public/deployment-parameters.json",
    MIDGARD_BLUEPRINT_CLASSIFICATION_FILE:
      "/run/public/blueprint-classification.json",
  };
  if (!/^[0-9a-f]{64}$/u.test(nodeEnv.MIDGARD_PUBLIC_MANIFEST_ID ?? ""))
    throw new Error("E_PUBLIC_COMPOSE_MANIFEST");
  const mounts = [
    ["MIDGARD_CONFIG_FILE", nodeEnv.MIDGARD_CONFIG_FILE],
    ["MIDGARD_PUBLIC_TLS_CERT_FILE", nodeEnv.MIDGARD_PUBLIC_TLS_CERT_FILE],
    ["MIDGARD_PUBLIC_TLS_KEY_FILE", nodeEnv.MIDGARD_PUBLIC_TLS_KEY_FILE],
    [
      "MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH",
      nodeEnv.MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH,
    ],
    [
      "MIDGARD_PUBLIC_RELEASE_AUTHORITY_FILE",
      nodeEnv.MIDGARD_PUBLIC_RELEASE_AUTHORITY_FILE,
    ],
    [
      "MIDGARD_PUBLIC_RELEASE_PROVENANCE_FILE",
      nodeEnv.MIDGARD_PUBLIC_RELEASE_PROVENANCE_FILE,
    ],
    [
      "MIDGARD_PUBLIC_RELEASE_TRUST_ROOTS_FILE",
      nodeEnv.MIDGARD_PUBLIC_RELEASE_TRUST_ROOTS_FILE,
    ],
    [
      "MIDGARD_PUBLIC_FINALIZED_MANIFEST_FILE",
      nodeEnv.MIDGARD_PUBLIC_FINALIZED_MANIFEST_FILE,
    ],
    [
      "MIDGARD_DEPLOYMENT_PARAMETER_REGISTRY_FILE",
      nodeEnv.MIDGARD_DEPLOYMENT_PARAMETER_REGISTRY_FILE,
    ],
    [
      "MIDGARD_BLUEPRINT_CLASSIFICATION_FILE",
      nodeEnv.MIDGARD_BLUEPRINT_CLASSIFICATION_FILE,
    ],
  ].map(([name, target]) => bind(mounted(env, name), target));
  const node = {
    ...restrictions,
    image: roleImages.node,
    environment: nodeEnv,
    user: "10001:10001",
    command: ["node", "./dist/index.js", "listen"],
    mem_limit: "32g",
    cpus: 16,
    restart: "unless-stopped",
    volumes: [
      ...mounts,
      { type: "volume", source: "node-data", target: "/app/db" },
    ],
    tmpfs: ["/tmp:rw,noexec,nosuid,size=256m,uid=10001,gid=10001"],
    networks: {
      public_backend: { ipv4_address: "10.91.7.3" },
      operator_egress: {},
    },
    depends_on: {
      postgres: { condition: "service_healthy" },
      migrate: { condition: "service_completed_successfully" },
    },
  };
  return {
    "x-midgard-public-http-overlay": {
      schema: "midgard-public-http-overlay-v1",
      requiredFullStackInputs: [
        "authenticated producer-container-committee-host manifest at /app/stack/producer.json",
        "mounted DA_LIBP2P_PRIVATE_KEY_SOURCE and reachable existing host-network committee peers",
        "Cardano node.socket at /ipc/node.socket with verified UID/GID access",
        "read-only actual Cardano config at /cardano-config/config.json",
        "pinned native L1 transport binary supplied by the public image or full-stack mount",
      ],
    },
    name: `midgard-public-${createHash("sha256")
      .update(fileURLToPath(new URL("../../..", import.meta.url)))
      .digest("hex")
      .slice(0, 8)}`,
    services: {
      node,
      migrate: {
        ...node,
        command: ["node", "./dist/index.js", "db:migrate"],
        networks: {
          public_backend: { ipv4_address: "10.91.7.5" },
          operator_egress: {},
        },
        restart: "no",
        depends_on: { postgres: { condition: "service_healthy" } },
      },
      postgres: {
        ...restrictions,
        image: roleImages.postgres,
        read_only: false,
        cap_add: ["CHOWN", "DAC_OVERRIDE", "SETUID", "SETGID"],
        environment: {
          POSTGRES_USER: "midgard",
          POSTGRES_DB: "midgard",
          POSTGRES_PASSWORD_FILE: "/run/secrets/postgres-password",
          PGDATA: "/var/lib/postgresql/data/pgdata",
        },
        volumes: [
          bind(
            mounted(env, "MIDGARD_PUBLIC_POSTGRES_PASSWORD_FILE"),
            "/run/secrets/postgres-password",
          ),
          {
            type: "volume",
            source: "postgres-data",
            target: "/var/lib/postgresql/data",
          },
        ],
        mem_limit: "8g",
        cpus: 4,
        restart: "unless-stopped",
        networks: { public_backend: { ipv4_address: "10.91.7.4" } },
        healthcheck: {
          test: [
            "CMD",
            "pg_isready",
            "-U",
            "midgard",
            "-d",
            "midgard",
            "-h",
            "127.0.0.1",
          ],
          interval: "5s",
          timeout: "3s",
          retries: 30,
        },
      },
      ingress: {
        ...restrictions,
        image: roleImages.ingress,
        user: "10001:10001",
        command: ["nginx", "-g", "daemon off;"],
        entrypoint: [],
        mem_limit: "1g",
        cpus: 2,
        pids_limit: 32,
        restart: "unless-stopped",
        ports: ["443:8443"],
        volumes: [
          bind(
            mounted(env, "MIDGARD_PUBLIC_INGRESS_CONFIG_FILE"),
            "/etc/nginx/nginx.conf",
          ),
          bind(
            mounted(env, "MIDGARD_PUBLIC_TLS_CERT_FILE"),
            "/run/public/tls-cert.pem",
          ),
          bind(
            mounted(env, "MIDGARD_PUBLIC_TLS_KEY_FILE"),
            "/run/secrets/tls-key.pem",
          ),
          bind(
            mounted(env, "MIDGARD_PUBLIC_TLS_CA_FILE"),
            "/run/public/tls-ca.pem",
          ),
        ],
        tmpfs: ["/tmp:rw,noexec,nosuid,size=32m,uid=10001,gid=10001"],
        networks: { public_backend: { ipv4_address: "10.91.7.2" } },
        depends_on: { node: { condition: "service_started" } },
      },
    },
    volumes: { "node-data": {}, "postgres-data": {} },
    networks: {
      public_backend: {
        internal: true,
        ipam: { config: [{ subnet: "10.91.7.0/28" }] },
      },
      operator_egress: {},
    },
  };
};

/**
 * Health, readiness and admin routes get a per-IP zone of their own and no
 * global zone, so a public flood that drains the global zone cannot refuse
 * them; a location's own limit_req replaces the server-level ones.
 */
export const renderPublicIngress = (env) => {
  const hostname = publicApiHostname(env);
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
    location = /submit { client_max_body_size 64m; proxy_pass https://node:3000; }
    location = /tx/evaluate { client_max_body_size 64m; proxy_pass https://node:3000; }
    location ~ ^/(healthz|readyz|init|commit|merge|stateQueue|logBlocksDB|logGlobals|operator/status|admin/session)$ { limit_req zone=liveness_ip burst=30 nodelay; proxy_pass https://node:3000; }
    location / { proxy_pass https://node:3000; }
  }
}
`;
};

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const [format, output] = process.argv.slice(2);
  if (
    !["compose", "ingress"].includes(format) ||
    typeof output !== "string" ||
    !isAbsolute(output)
  )
    throw new Error(
      "Use public-operator-compose.mjs compose|ingress ABSOLUTE_OUTPUT_PATH",
    );
  const content =
    format === "compose"
      ? JSON.stringify(renderPublicOperatorCompose(process.env), null, 2) + "\n"
      : renderPublicIngress(process.env);
  writeFileSync(output, content, { flag: "wx", mode: 0o600 });
}
