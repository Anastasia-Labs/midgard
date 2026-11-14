/**
 * Loaded with `node --import` into node commands that the full stack runs
 * against a preprod-public node, so their fetches reach its HTTPS port.
 */
import {
  NODE_TLS_CA_ENV,
  NODE_TLS_SERVERNAME_ENV,
  trustNodeTls,
} from "./full-stack/node-tls.js";

const caFile = process.env[NODE_TLS_CA_ENV];
const servername = process.env[NODE_TLS_SERVERNAME_ENV];
if (caFile && servername) trustNodeTls(caFile, servername);
