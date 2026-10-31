import "node:fs";
import "node:path";
import "node:url";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/lucid-data";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "@/index.js";
import "../src/fraud-proof/contracts/cek-core.js";
import "../src/index.js";
import "./fault-proof.publication-tx-overhead-bytes.js";
import "./fault-proof.fault-proof-abi.js";
import "./fault-proof.double-spend-fault-proof-contract-builder.js";
import "./fault-proof.fault-proof-contract-builder.js";

import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { beforeAll } from "vitest";

// The ledger's Ed25519 check is libsodium's, which initialises
// asynchronously; production entry points await it, and so does this file.
beforeAll(() => ledgerEd25519SignatureReady());
