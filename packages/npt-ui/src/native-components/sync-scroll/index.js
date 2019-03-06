"use strict";

export default class SyncScroll {
    constructor(node1, node2, options) {
        const defaults = {
            axisX: true,
            axisY: true,
            twoWay: false
        };

        const getClass = Object.prototype.toString;
        const allowedType = "HTML";

        this.options = Object.assign({}, defaults, options);

        if (
            !~getClass.call(node1).indexOf(allowedType) ||
            !~getClass.call(node1).indexOf(allowedType)
        ) {
            throw new Error("Only HTML Nodes allowed!");
        }

        this.node1 = node1;
        this.node2 = node2;

        this.handleScroll = this.handleScroll.bind(this);
        this.attachScroll = this.attachScroll.bind(this);
        this.detachScroll = this.detachScroll.bind(this);
    }

    init() {
        this.attachEvents();
        this.sync(this.node1, this.node2);
        this.sync(this.node2, this.node1);
    }

    remove() {
        this.detachEvents();
    }

    handleScroll(e) {
        if (e.target === this.node1) {
            this.sync(this.node1, this.node2);
        } else {
            this.sync(this.node2, this.node1);
        }
    }

    sync(srcNode, destNode) {
        this.options.axisY && (destNode.scrollTop = srcNode.scrollTop);
        this.options.axisX && (destNode.scrollLeft = srcNode.scrollLeft);
    }

    attachEvents() {
        this.attachScroll({ target: this.node1 });
        // this.node1.addEventListener("mouseenter", this.attachScroll);
        // this.node1.addEventListener("mouseleave", this.detachScroll);

        if (this.options.twoWay) {
            this.attachScroll({ target: this.node2 });
            // this.node2.addEventListener("mouseenter", this.attachScroll);
            // this.node2.addEventListener("mouseleave", this.detachScroll);
        }
    }

    attachScroll({ target }) {
        target.addEventListener("scroll", this.handleScroll);
    }

    detachScroll({ target }) {
        target.removeEventListener("scroll", this.handleScroll);
    }

    detachEvents() {
        // this.node1.removeEventListener("mouseenter", this.attachScroll);
        // this.node1.removeEventListener("mouseleave", this.detachScroll);
        // this.node2.removeEventListener("mouseenter", this.attachScroll);
        // this.node2.removeEventListener("mouseleave", this.detachScroll);
        this.node1.removeEventListener("scroll", this.handleScroll);
        this.node2.removeEventListener("scroll", this.handleScroll);
    }
}
