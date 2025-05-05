package controller

import (
	"context"
	"encoding/hex"
	"fmt"
	"log"
	"net/http"
	"strings"

	"github.com/Anastasia-Labs/midgard-node-go/config"
	"github.com/Anastasia-Labs/midgard-node-go/entity"
	"github.com/Salvionied/apollo"
	"github.com/Salvionied/apollo/constants"
	"github.com/Salvionied/apollo/txBuilding/Backend/BlockFrostChainContext"
	"github.com/fxamacker/cbor"
	"github.com/gin-gonic/gin"
	"go.uber.org/zap"
)

type NodeController struct {
	chainCtx   BlockFrostChainContext.BlockFrostChainContext
	seedPhrase string
	node       *entity.Node
	mempool    *entity.Mempool
	peers      []string
	metrics    entity.Metrics
	logger     *zap.SugaredLogger
	ctx        context.Context
}

func NewNodeController(cfg *config.NodeConfig, ctx context.Context, logger *zap.SugaredLogger) (*NodeController, error) {
	nodectl := &NodeController{}
	nodectl.ctx = ctx
	nodectl.logger = logger
	nodectl.peers = cfg.Peers
	nodectl.seedPhrase = cfg.SeedPhrase
	// TODO: load from db
	nodectl.mempool = entity.NewMempool()
	node, err := entity.NewNode(ctx, nodectl.mempool, cfg.Peers, cfg.CryptoKeyPath, logger)
	if err != nil {
		log.Fatal(err)
	}
	nodectl.node = node
	var (
		providerUrl string
		networkId   int
	)
	switch strings.ToLower(cfg.Network) {
	case "mainnet":
		{
			providerUrl = constants.BLOCKFROST_BASE_URL_MAINNET
			networkId = int(constants.MAINNET)
		}
	case "testnet":
		{
			providerUrl = constants.BLOCKFROST_BASE_URL_TESTNET
			networkId = int(constants.TESTNET)
		}
	case "preview":
		{
			providerUrl = constants.BLOCKFROST_BASE_URL_PREVIEW
			networkId = int(constants.PREVIEW)
		}
	default:
		{
			providerUrl = constants.BLOCKFROST_BASE_URL_PREPROD
			networkId = int(constants.PREPROD)
		}
	}
	nodectl.chainCtx, err = BlockFrostChainContext.NewBlockfrostChainContext(providerUrl, networkId, cfg.ProviderKey)
	if err != nil {
		logger.Error("BlockFrostChainContext", zap.Error(err))
		return nil, err
	}
	return nodectl, nil
}

func (n *NodeController) RegisterMetrics(opts *entity.MetricsOpts) *entity.Metrics {
	n.metrics = entity.BuildMetrics(opts)
	n.logger.Info("api metrics ", zap.Any("opts", opts))
	return &n.metrics
}

func (n *NodeController) SubmitTx(c *gin.Context) {
	var tx entity.Transaction
	if err := c.ShouldBindJSON(&tx); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": "Invalid TX"})
		return
	}
	// Validate tx
	n.mempool.Add(tx)
	if err := n.node.Broadcast(tx); err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": "Broadcast fail"})
		return
	}
	c.JSON(http.StatusAccepted, gin.H{"status": "OK"})
}

func (n *NodeController) GetMempool(c *gin.Context) {
	c.JSON(http.StatusOK, n.mempool.All())
}

func (n *NodeController) InitProtocol(c *gin.Context) {
	cc := apollo.NewEmptyBackend()

	builder := apollo.New(&cc)
	builder, err := builder.SetWalletFromMnemonic(n.seedPhrase, constants.PREPROD)
	builder, err = builder.SetWalletAsChangeAddress()

	utxos, err := n.chainCtx.Utxos(*builder.GetWallet().GetAddress())
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"Utxos": err.Error()})
		return
	}
	builder, err = builder.
		AddLoadedUTxOs(utxos...).
		PayToAddressBech32("addr_test1qq7qwehzy0ynscvcexe9ha3qehdvrlm50kdkv9vc45t9phccvc9ujer30k5azhkm5jfun87uzvx829cft2j8namukunsqvk4ux", 1_000_000).
		Complete()
	if err != nil {
		fmt.Println(err)
	}
	builder = builder.Sign()
	tx := builder.GetTx()
	cborred, err := cbor.Marshal(tx, cbor.CanonicalEncOptions())
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(hex.EncodeToString(cborred))
	// tx_id, _ := bfc.SubmitTx(*tx)

	// fmt.Println(hex.EncodeToString(tx_id.Payload))
	c.JSON(http.StatusOK, hex.EncodeToString(cborred))
}

func (n *NodeController) Stop() {
	n.node.Sub.Cancel()
	n.node.Host.Close()
}
