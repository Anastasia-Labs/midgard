package controller

import (
	"context"
	"log"
	"net/http"

	"github.com/Anastasia-Labs/midgard-node-go/entity"
	"github.com/gin-gonic/gin"
	"go.uber.org/zap"
)

type NodeController struct {
	node    *entity.Node
	mempool *entity.Mempool
	peers   []string
	metrics entity.Metrics
	logger  *zap.SugaredLogger
	ctx     context.Context
}

func NewNodeController(peers []string, cryptoKeyPath string, ctx context.Context, logger *zap.SugaredLogger) (*NodeController, error) {
	// TODO: load from db
	mempool := entity.NewMempool()
	node, err := entity.NewNode(ctx, mempool, peers, cryptoKeyPath, logger)
	if err != nil {
		log.Fatal(err)
	}
	return &NodeController{
		node:    node,
		mempool: mempool,
		peers:   peers,
		logger:  logger,
		ctx:     ctx,
	}, nil
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

func (n *NodeController) Stop() {
	n.node.Sub.Cancel()
	n.node.Host.Close()
}
