import { strict as assert } from "node:assert";
import { test } from "node:test";

import { renderPublicIngress } from "./public-ingress.mjs";

const env = { MIDGARD_PUBLIC_API_ORIGIN: "https://node.example.test" };

test("public ingress forwards only named headers over verified TLS and logs no client data", () => {
  const nginx = renderPublicIngress(env);
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
});

test("public ingress gives health, readiness and admin routes their own rate zone", () => {
  const nginx = renderPublicIngress({ ...env, PORT: "3443" });
  const regexLocations = [...nginx.matchAll(/location ~ (\S+) \{([^}]*)\}/gu)];
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
    // The stack's node service on its own port.
    assert.ok(location[2].includes("proxy_pass https://midgard-node:3443;"));
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
});

test("public ingress refuses an origin with a port or path, and an invalid node port", () => {
  for (const origin of ["https://node.test:3000", "https://node.test/path"])
    assert.throws(
      () => renderPublicIngress({ MIDGARD_PUBLIC_API_ORIGIN: origin }),
      /E_PUBLIC_INGRESS_ORIGIN/u,
    );
  assert.throws(
    () => renderPublicIngress({ ...env, PORT: "3000;evil" }),
    /E_PUBLIC_INGRESS_PORT/u,
  );
});
