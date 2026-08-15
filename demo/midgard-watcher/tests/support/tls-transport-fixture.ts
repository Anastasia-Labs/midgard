/**
 * The throwaway TLS endpoint the authenticated indexer suites attest against.
 *
 * Every suite that exercises `establishWatcherExternalProviderTransportV1`
 * needs a real listening TLS server whose certificate it can pin, because the
 * transport layer refuses to attest a provider it cannot handshake with.
 * Minting one is the same twenty lines of `openssl` invocation in every suite,
 * so they were copied four times over.
 *
 * The registry itself (which directory, which servers, which attestation
 * contexts) stays with the caller: each suite tears its own down in its own
 * `afterAll`, and the suites disagree about what they name things.
 */
import { execFile } from "node:child_process";
import { createHash, X509Certificate } from "node:crypto";
import { readFile } from "node:fs/promises";
import { type Server } from "node:net";
import { join } from "node:path";
import { createServer as createTlsServer } from "node:tls";
import { promisify } from "node:util";

const execFileAsync = promisify(execFile);

/** Resolve once the server is listening, without leaving the rejection
 * handler attached to leak into later errors. */
export const listen = async (
  server: Server,
  target: string | number,
): Promise<void> =>
  await new Promise((resolve, reject) => {
    server.once("error", reject);
    const onListen = () => {
      server.off("error", reject);
      resolve();
    };
    if (typeof target === "string") {
      server.listen(target, onListen);
    } else {
      server.listen(target, "127.0.0.1", onListen);
    }
  });

export type WatcherTlsTransportFixtureV1 = Readonly<{
  certificate: string;
  identitySha256: string;
  port: number;
}>;

/**
 * Mint a self-signed localhost certificate, serve it, and report the SHA-256
 * the caller should pin as the transport's public identity.
 *
 * The started server is pushed onto `servers` so the caller's `afterAll` can
 * close it.
 */
export const makeWatcherTlsTransportFixtureV1 = async (
  directory: string,
  servers: Server[],
  name: string,
): Promise<WatcherTlsTransportFixtureV1> => {
  const keyPath = join(directory, `${name}.key`);
  const certificatePath = join(directory, `${name}.crt`);
  await execFileAsync("openssl", [
    "req",
    "-x509",
    "-newkey",
    "rsa:2048",
    "-nodes",
    "-keyout",
    keyPath,
    "-out",
    certificatePath,
    "-days",
    "1",
    "-subj",
    "/CN=localhost",
    "-addext",
    "subjectAltName=DNS:localhost",
  ]);
  const [key, certificate] = await Promise.all([
    readFile(keyPath, "utf8"),
    readFile(certificatePath, "utf8"),
  ]);
  const server = createTlsServer({ key, cert: certificate });
  await listen(server, 0);
  servers.push(server);
  const address = server.address();
  if (address === null || typeof address === "string") {
    throw new Error("TLS fixture did not bind a TCP port");
  }
  return {
    certificate,
    identitySha256: createHash("sha256")
      .update(new X509Certificate(certificate).raw)
      .digest("hex"),
    port: address.port,
  };
};
