import { strict as assert } from "node:assert";
import { chmodSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";

import {
  renderPublicIngress,
  renderPublicOperatorCompose,
} from "./public-operator-compose.mjs";

const fixture = () => {
  const root = mkdtempSync(join(tmpdir(), "midgard-r7-render-"));
  const config = join(root, "config.json");
  const values = {
    PORT: "3000",
    POSTGRES_HOST: "postgres",
    POSTGRES_PORT: "5432",
    POSTGRES_USER: "midgard",
    POSTGRES_DB: "midgard",
    HTTP_TRUSTED_PROXY_PEERS: "10.91.7.2",
  };
  writeFileSync(config, JSON.stringify(values), { mode: 0o600 });
  const env = {
    MIDGARD_CONFIG_FILE: config,
    MIDGARD_PUBLIC_API_ORIGIN: "https://node.example.test",
    MIDGARD_PUBLIC_RELEASE_REVISION: "a".repeat(40),
    MIDGARD_PUBLIC_MANIFEST_ID: "b".repeat(64),
  };
  for (const role of ["NODE", "POSTGRES", "INGRESS"])
    env[`MIDGARD_PUBLIC_${role}_IMAGE`] =
      `example.invalid/${role.toLowerCase()}@sha256:${"c".repeat(64)}`;
  for (const name of [
    "MIDGARD_DEPLOYMENT_PARAMETER_REGISTRY_FILE",
    "MIDGARD_BLUEPRINT_CLASSIFICATION_FILE",
    "MIDGARD_PUBLIC_TLS_CERT_FILE",
    "MIDGARD_PUBLIC_TLS_KEY_FILE",
    "MIDGARD_PUBLIC_TLS_CA_FILE",
    "MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH",
    "MIDGARD_PUBLIC_RELEASE_AUTHORITY_FILE",
    "MIDGARD_PUBLIC_RELEASE_PROVENANCE_FILE",
    "MIDGARD_PUBLIC_RELEASE_TRUST_ROOTS_FILE",
    "MIDGARD_PUBLIC_FINALIZED_MANIFEST_FILE",
    "MIDGARD_PUBLIC_POSTGRES_PASSWORD_FILE",
    "MIDGARD_PUBLIC_INGRESS_CONFIG_FILE",
  ])
    env[name] = join(root, name);
  return { root, config, values, env };
};

test("public rendering publishes only TLS and cannot inherit demo services, credentials or host mounts", () => {
  const f = fixture();
  try {
    const rendered = renderPublicOperatorCompose(f.env);
    assert.equal(
      rendered["x-midgard-public-http-overlay"].schema,
      "midgard-public-http-overlay-v1",
    );
    assert.equal(
      rendered["x-midgard-public-http-overlay"].requiredFullStackInputs.length,
      5,
    );
    assert.deepEqual(Object.keys(rendered.services).sort(), [
      "ingress",
      "migrate",
      "node",
      "postgres",
    ]);
    assert.deepEqual(
      Object.values(rendered.services).flatMap(
        (service) => service.ports ?? [],
      ),
      ["443:8443"],
    );
    assert.equal(rendered.services.node.mem_limit, "32g");
    assert.equal(rendered.services.node.cpus, 16);
    assert.equal(rendered.services.node.user, "10001:10001");
    assert.equal(rendered.services.ingress.user, "10001:10001");
    assert.notEqual(
      rendered.services.node.networks.public_backend.ipv4_address,
      rendered.services.migrate.networks.public_backend.ipv4_address,
    );
    assert.equal(rendered.networks.public_backend.internal, true);
    for (const service of Object.values(rendered.services)) {
      assert.ok(service.image.includes("@sha256:"));
      assert.ok(service.security_opt.includes("no-new-privileges:true"));
      assert.equal(service.privileged, undefined);
      assert.equal(service.network_mode, undefined);
      assert.equal(service.env_file, undefined);
      for (const volume of service.volumes ?? [])
        if (volume.type === "bind") {
          assert.ok(volume.source.startsWith(f.root + "/"));
          assert.equal(volume.read_only, true);
          assert.equal(volume.bind.create_host_path, false);
        }
    }
    assert.deepEqual(rendered.services.node.command, [
      "node",
      "./dist/index.js",
      "listen",
    ]);
    assert.equal(
      rendered.services.postgres.environment.POSTGRES_PASSWORD,
      undefined,
    );
    assert.equal(
      rendered.services.postgres.environment.POSTGRES_PASSWORD_FILE,
      "/run/secrets/postgres-password",
    );
    const nginx = renderPublicIngress(f.env);
    assert.ok(
      nginx.includes("proxy_set_header X-Midgard-Client-IP $remote_addr"),
    );
    assert.ok(nginx.includes("proxy_pass_request_headers off"));
    assert.ok(
      nginx.includes(
        "proxy_set_header X-Midgard-Admin-Key $http_x_midgard_admin_key",
      ),
    );
    assert.ok(nginx.includes("proxy_ssl_verify on"));
    assert.ok(nginx.includes("nodelay"));
    assert.ok(nginx.includes("client_max_body_size 64m"));
    const log = nginx.match(/log_format public[^;]+;/u)?.[0];
    assert.ok(log);
    for (const raw of [
      "$request_uri",
      "$args",
      "$request_body",
      "$remote_addr",
      "$request;",
    ])
      assert.ok(!log.includes(raw));
  } finally {
    rmSync(f.root, { recursive: true, force: true });
  }
});

test("public ingress gives health, readiness and admin routes their own rate zone", () => {
  const f = fixture();
  try {
    const nginx = renderPublicIngress(f.env);
    const regexLocations = [
      ...nginx.matchAll(/location ~ (\S+) \{([^}]*)\}/gu),
    ];
    const routeOf = (path) =>
      regexLocations.find(([, pattern]) => new RegExp(pattern, "u").test(path));
    const liveness = [
      "/healthz",
      "/readyz",
      "/init",
      "/commit",
      "/merge",
      "/stateQueue",
      "/logBlocksDB",
      "/logGlobals",
      "/operator/status",
      "/admin/session",
    ];
    for (const path of liveness) {
      const location = routeOf(path);
      assert.ok(location, `${path} has no liveness location`);
      const zones = [...location[2].matchAll(/limit_req zone=(\w+)/gu)].map(
        ([, zone]) => zone,
      );
      // A location's own limit_req replaces the server's, so the public
      // global zone does not apply here.
      assert.equal(zones.length, 1);
      assert.ok(!["per_ip", "global"].includes(zones[0]));
      assert.ok(
        nginx.includes(`limit_req_zone $binary_remote_addr zone=${zones[0]}:`),
      );
      assert.ok(location[2].includes("proxy_pass https://node:3000;"));
    }
    for (const path of [
      "/tx",
      "/utxos",
      "/tx-status",
      "/submit",
      "/tx/evaluate",
      "/healthz/x",
      "/xreadyz",
    ])
      assert.equal(routeOf(path), undefined, path);
  } finally {
    rmSync(f.root, { recursive: true, force: true });
  }
});

test("public rendering refuses mutable roles, injected origin and topology/proxy mismatch", () => {
  const f = fixture();
  try {
    for (const role of ["NODE", "POSTGRES", "INGRESS"])
      assert.throws(
        () =>
          renderPublicOperatorCompose({
            ...f.env,
            [`MIDGARD_PUBLIC_${role}_IMAGE`]: "example.invalid/image:latest",
          }),
        /E_PUBLIC_COMPOSE_IMAGE/u,
      );
    assert.throws(
      () =>
        renderPublicIngress({
          ...f.env,
          MIDGARD_PUBLIC_API_ORIGIN: "https://node.test:3000",
        }),
      /E_PUBLIC_COMPOSE_ORIGIN/u,
    );
    assert.throws(
      () =>
        renderPublicIngress({
          ...f.env,
          MIDGARD_PUBLIC_API_ORIGIN: "https://node.test/path",
        }),
      /E_PUBLIC_COMPOSE_ORIGIN/u,
    );
    for (const invalid of [
      { ...f.values, HTTP_TRUSTED_PROXY_PEERS: "198.51.100.1" },
      { ...f.values, POSTGRES_HOST: "outside.test" },
    ]) {
      writeFileSync(f.config, JSON.stringify(invalid));
      assert.throws(
        () => renderPublicOperatorCompose(f.env),
        /E_PUBLIC_COMPOSE_CONFIG/u,
      );
    }
    writeFileSync(f.config, JSON.stringify(f.values));
    chmodSync(f.config, 0o644);
    assert.throws(
      () => renderPublicOperatorCompose(f.env),
      /E_PUBLIC_COMPOSE_CONFIG/u,
    );
  } finally {
    rmSync(f.root, { recursive: true, force: true });
  }
});
