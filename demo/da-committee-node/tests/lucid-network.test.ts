/**
 * Every committee Lucid construction site on a `Custom` network: the slot
 * mapping comes from the site's own Ogmios (network magic, Shelley genesis,
 * submit-slot snapshot) or the client is refused by name before Lucid is
 * built. Named networks make no Ogmios slot query. The Ogmios here is a local
 * HTTP JSON-RPC fake.
 */

import "node:http";
import "node:path";
import "@lucid-evolution/lucid";
import "vitest";
import "../src/availability/factory.js";
import "../src/coordinator/factory.js";
import "../src/l1/da-attestation-reader.js";
import "../src/l1/lucid.js";
import "../src/l1/lucid-network.js";
import "../src/l1/provider.js";
import "./helpers.js";
import "./lucid-network.start-fake-ogmios.js";
import "./lucid-network.the-l1-factories-main-calls-build-custom-lucid-on-the-genesis-mapping.js";
import "./lucid-network.the-live-l1-reads-main-wires-prove-custom-identity-against-the-configured-magic.js";
