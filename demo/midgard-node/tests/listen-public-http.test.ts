import { execFileSync } from "node:child_process";
import { once } from "node:events";
import {
  chmodSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { request as httpsRequest } from "node:https";
import type { AddressInfo } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { generateMnemonic } from "bip39";
import { Cause, Effect, Exit, Logger } from "effect";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { cliExitCode } from "../src/commands/cli-runtime.js";
import {
  authenticatePublicHttpRelease,
  type PublicHttpProfile,
  publicHttpTransport,
} from "../src/commands/listen-public-http.js";
import {
  createPublicLogRedactor,
  redactPublicLogger,
} from "../src/commands/listen-public-logging.js";
import {
  type NodeConfigDep,
  seedPhraseAddress,
} from "../src/services/config.node-config-dep.js";
import { ValidationWorkerError } from "../src/services/validation-pool.js";

const directory = mkdtempSync(join(tmpdir(), "midgard-r7-tls-"));
const configPath = join(directory, "config.yaml");
const certificate = join(directory, "certificate.pem");
const privateKey = join(directory, "private-key.pem");
const identity = join(directory, "deployment.json");
const operator = generateMnemonic();
const merge = generateMnemonic();
const reference = generateMnemonic();
const config = {
  ADMIN_API_KEY: "0123456789abcdef".repeat(4),
  POSTGRES_PASSWORD: "r7-testing-password-with-private-entropy",
  L1_OPERATOR_SEED_PHRASE: operator,
  L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: merge,
  L1_REFERENCE_SCRIPT_SEED_PHRASE: reference,
  L1_SETTLEMENT_SEED_PHRASE: generateMnemonic(),
  NETWORK: "Preprod",
  PORT: 3000,
  DEPLOYMENT_ECONOMICS_PROFILE: "public-preprod-launch-v1",
  RUN_GENESIS_ON_STARTUP: false,
  VALIDATION_WORKER_POOL_SIZE: 2,
} as NodeConfigDep;
// Fixture identities replace the public allow-list only inside this injected profile test.
const profile: PublicHttpProfile = {
  ...DEPLOYMENT_PROFILES["preprod-public"],
  allow_lists: {
    ...DEPLOYMENT_PROFILES["preprod-public"].allow_lists,
    operators: [operator, merge].map(
      (seed) =>
        getAddressDetails(seedPhraseAddress(seed, "Preprod")).paymentCredential!
          .hash,
    ),
  },
};
const env: NodeJS.ProcessEnv = {
  MIDGARD_DOTENV_MODE: "disabled",
  MIDGARD_CONFIG_FILE: configPath,
  MIDGARD_PUBLIC_RELEASE_REVISION: "a".repeat(40),
  MIDGARD_PUBLIC_NODE_IMAGE:
    "example.invalid/operator@sha256:" + "a".repeat(64),
  MIDGARD_PUBLIC_INGRESS_IMAGE:
    "example.invalid/ingress@sha256:" + "c".repeat(64),
  MIDGARD_PUBLIC_POSTGRES_IMAGE:
    "example.invalid/postgres@sha256:" + "b".repeat(64),
  MIDGARD_PUBLIC_API_ORIGIN: "https://node.test",
  MIDGARD_PUBLIC_TLS_CERT_FILE: certificate,
  MIDGARD_PUBLIC_TLS_KEY_FILE: privateKey,
  MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: identity,
};
const testOnlyReleaseVerifier = () => undefined; // Isolates transport; compiled public release authentication is a separate required gate.
const mount = (value = config) =>
  writeFileSync(
    configPath,
    JSON.stringify(
      Object.fromEntries(
        Object.entries(value).map(([key, entry]) => [key, String(entry)]),
      ),
    ),
    { mode: 0o600 },
  );

beforeAll(() => {
  execFileSync(
    "openssl",
    [
      "req",
      "-x509",
      "-newkey",
      "rsa:2048",
      "-nodes",
      "-keyout",
      privateKey,
      "-out",
      certificate,
      "-days",
      "2",
      "-subj",
      "/CN=node.test",
      "-addext",
      "subjectAltName=DNS:node.test",
    ],
    { stdio: "ignore" },
  );
  chmodSync(privateKey, 0o600);
  writeFileSync(identity, "{}", { mode: 0o600 });
  mount();
});
afterAll(() => rmSync(directory, { recursive: true, force: true }));

describe("public HTTP local preflight and TLS", () => {
  it.each([
    ["MIDGARD_CONFIG_FILE", undefined],
    ["MIDGARD_DOTENV_MODE", "enabled"],
    ["MIDGARD_PUBLIC_TLS_KEY_FILE", undefined],
    ["MIDGARD_PUBLIC_API_ORIGIN", "http://node.test"],
    ["MIDGARD_PUBLIC_NODE_IMAGE", "operator:latest"],
    ["MIDGARD_PUBLIC_RELEASE_REVISION", "main"],
    ["MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH", undefined],
  ])("refuses %s before constructing any server", (name, value) => {
    expect(() =>
      publicHttpTransport(config, {
        env: { ...env, [name]: value },
        profile,
        testOnlyReleaseVerifier,
      }),
    ).toThrow("E_PUBLIC_HTTP_CONFIG");
  });
  it("calls the real default release verifier before constructing the listener", () => {
    mount();
    expect(() => publicHttpTransport(config, { env, profile })).toThrow(
      "E_PUBLIC_HTTP_CONFIG",
    );
    expect(() => authenticatePublicHttpRelease(env)).toThrow(
      "E_PUBLIC_HTTP_CONFIG",
    );
  });
  it("refuses blank keys, same publisher/operator and user custody", () => {
    for (const invalid of [
      { ...config, ADMIN_API_KEY: "" },
      { ...config, POSTGRES_PASSWORD: "postgres" },
      { ...config, L1_REFERENCE_SCRIPT_SEED_PHRASE: operator },
      { ...config, VALIDATION_WORKER_POOL_SIZE: 1 },
    ]) {
      mount(invalid);
      expect(() =>
        publicHttpTransport(invalid, { env, profile, testOnlyReleaseVerifier }),
      ).toThrow("E_PUBLIC_HTTP_CONFIG");
    }
    mount();
    expect(() =>
      publicHttpTransport(config, {
        env: { ...env, L1_USER_SEED_PHRASE: generateMnemonic() },
        profile,
        testOnlyReleaseVerifier,
      }),
    ).toThrow("user wallet");
  });
  it("refuses environment secret overrides and public wallet identities outside the allow-list", () => {
    mount();
    expect(() =>
      publicHttpTransport(
        { ...config, ADMIN_API_KEY: "ff".repeat(32) },
        { env, profile, testOnlyReleaseVerifier },
      ),
    ).toThrow("mounted configuration");
    expect(() =>
      publicHttpTransport(config, {
        env,
        profile: {
          ...profile,
          allow_lists: { ...profile.allow_lists, operators: ["00".repeat(28)] },
        },
        testOnlyReleaseVerifier,
      }),
    ).toThrow("allow-list");
  });
  it("refuses mismatched hostname and publicly readable key files", () => {
    mount();
    expect(() =>
      publicHttpTransport(config, {
        env: { ...env, MIDGARD_PUBLIC_API_ORIGIN: "https://other.test" },
        profile,
        testOnlyReleaseVerifier,
      }),
    ).toThrow("TLS");
    chmodSync(privateKey, 0o644);
    try {
      expect(() =>
        publicHttpTransport(config, { env, profile, testOnlyReleaseVerifier }),
      ).toThrow("mounted");
    } finally {
      chmodSync(privateKey, 0o600);
    }
  });
  it("serves real TLS1.3 with the mounted identity and rejects plain HTTP", async () => {
    mount();
    const transport = publicHttpTransport(config, {
      env,
      profile,
      testOnlyReleaseVerifier,
    });
    const server = transport.createServer();
    server.on("request", (_request, response) => {
      response.writeHead(200, { "content-type": "application/json" });
      response.end('{"status":"ok"}');
    });
    server.listen(0, "127.0.0.1");
    await once(server, "listening");
    const port = (server.address() as AddressInfo).port;
    try {
      const reply = await new Promise<{ status: number; protocol: string }>(
        (resolve, reject) => {
          const request = httpsRequest(
            {
              hostname: "127.0.0.1",
              port,
              servername: "node.test",
              ca: readFileSync(certificate),
              minVersion: "TLSv1.3",
            },
            (response) => {
              const protocol = (
                response.socket as import("node:tls").TLSSocket
              ).getProtocol()!;
              response.resume();
              response.once("end", () =>
                resolve({ status: response.statusCode!, protocol }),
              );
            },
          );
          request.once("error", reject);
          request.end();
        },
      );
      expect(reply).toEqual({ status: 200, protocol: "TLSv1.3" });
      await expect(fetch(`http://127.0.0.1:${port}/healthz`)).rejects.toThrow();
    } finally {
      await new Promise<void>((resolve, reject) =>
        server.close((error) => (error ? reject(error) : resolve())),
      );
    }
  });
});

describe("public diagnostic rendering", () => {
  it("never republishes prefixed hex, base64 or free-form keys as public log codes or labels", async () => {
    const prefixedHex = "E_" + "AB".repeat(32);
    const base64 = Buffer.from(
      "raw provider credential and transaction bytes",
    ).toString("base64");
    const freeform =
      "untrusted provider field containing arbitrary private words";
    const rendered: string[] = [];
    const logger = redactPublicLogger(
      Logger.make((options) => {
        rendered.push(
          JSON.stringify({
            message: options.message,
            annotations: [...options.annotations],
            spans: [...options.spans].map((span) => span.label),
          }),
        );
      }),
      {},
    );
    await Effect.runPromise(
      Effect.logWarning(
        {
          error_code: "E_HTTP_INTERNAL",
          count: 7,
          [prefixedHex]: prefixedHex,
          [base64]: base64,
          [freeform]: freeform,
        },
        prefixedHex,
        base64,
        freeform,
      ).pipe(
        Effect.annotateLogs({
          error_code: "E_HTTP_INTERNAL",
          count: 7,
          [prefixedHex]: prefixedHex,
          [base64]: base64,
          [freeform]: freeform,
        }),
        Effect.withLogSpan(prefixedHex),
        Effect.provide(Logger.replace(Logger.defaultLogger, logger)),
      ),
    );
    const output = rendered.join("\n");
    expect(rendered).toHaveLength(1);
    for (const unsafe of [prefixedHex, base64, freeform])
      expect(output).not.toContain(unsafe);
    expect(output).toContain("E_HTTP_INTERNAL");
    expect(output).toContain('"count":7');
  });

  it("redacts secrets, bodies, addresses, provider URLs and CBOR without invoking getters", () => {
    const secret = "private fixture credential with spaces";
    const redact = createPublicLogRedactor({ ADMIN_API_KEY: secret });
    let getter = false;
    const input = {
      reason: "E_TYPED_REFUSAL",
      count: 7,
      seed: secret,
      body: Buffer.from("abcd", "hex"),
      provider: "https://provider.test/?token=" + secret,
      cbor: "abcd".repeat(100),
      address: "addr_test1" + "a".repeat(90),
      message: secret,
      get accidental() {
        getter = true;
        return secret;
      },
    };
    const output = JSON.stringify(redact.value(input));
    expect(output).toContain("E_TYPED_REFUSAL");
    expect(output).toContain('"count":7');
    for (const sensitive of [
      secret,
      "provider.test",
      "abcd".repeat(100),
      input.address,
    ])
      expect(output).not.toContain(sensitive);
    expect(getter).toBe(false);
  });
  it("redacts failure rendering while preserving the original error and CLI exit contract", async () => {
    const rendered: string[] = [];
    const secret = "private-test-credential";
    const logger = redactPublicLogger(
      Logger.make((options) => {
        rendered.push(
          JSON.stringify(options.message) +
            JSON.stringify([...options.annotations]) +
            Cause.pretty(options.cause),
        );
      }),
      { POSTGRES_PASSWORD: secret },
    );
    const original = new ValidationWorkerError({
      message: secret,
      cause: Buffer.alloc(64, 0xab),
    });
    const providerText =
      "an arbitrary upstream response containing private nonhex words";
    const base64 = Buffer.from(
      "raw user transaction contents from a provider",
    ).toString("base64");
    const blockHash = "ca".repeat(28);
    const effect = Effect.fail(original).pipe(
      Effect.tapErrorCause((cause) =>
        Effect.logError(
          "E_TX_EVALUATION_UNAVAILABLE",
          cause,
          providerText,
          base64,
        ).pipe(
          Effect.annotateLogs({
            header_hash: blockHash,
            count: 7,
            reason: "database_unreachable",
          }),
        ),
      ),
    );
    const exit = await Effect.runPromiseExit(
      effect.pipe(Effect.provide(Logger.replace(Logger.defaultLogger, logger))),
    );
    expect(Exit.isFailure(exit)).toBe(true);
    if (Exit.isFailure(exit))
      expect(Cause.failureOption(exit.cause)).toMatchObject({
        value: original,
      });
    expect(cliExitCode(exit, undefined)).toBe(1);
    expect(rendered.join(" ")).toContain("E_TX_EVALUATION_UNAVAILABLE");
    expect(rendered.join(" ")).not.toContain(secret);
    expect(rendered.join(" ")).not.toContain("ab".repeat(64));
    expect(rendered.join(" ")).not.toContain(providerText);
    expect(rendered.join(" ")).not.toContain(base64);
    expect(rendered.join(" ")).toContain(blockHash);
    expect(rendered.join(" ")).toContain("database_unreachable");
  });
});
