import app from "elm";
import $ from "jquery";
import { createDatePicker, createMonthPicker, SyncScroll } from "native-components";

const getDateRange = data => {
    setTimeout(function() {
        $(".page-holder.resources #reportrange").each(function() {
            createDatePicker($(this), data, (elem, detail) => {
                app.ports.onDatepickerPlanResChange.send(detail);
            });
        });
        $(".page-holder.workorders #reportrange").each(function() {
            createMonthPicker($(this), data, (elem, detail) => {
                app.ports.onDatepickerWOChange.send(detail);
            });
        });
        $(".page-holder.workorders-executive #reportrange").each(function() {
            createMonthPicker($(this), data, (elem, detail) => {
                app.ports.onDatepickerWOExecChange.send(detail);
            });
        });
    }, 60);
};

function sendScrollToPort() {
    let scrollFlag = true;
    let timer = null;

    return function(scroll) {
        scroll && scroll.preventDefault();

        if (scrollFlag) {
            app.ports.onResTableScroll.send(null);
            scrollFlag = false;
        }

        if (timer !== null) {
            clearTimeout(timer);
        }

        timer = setTimeout(() => {
            scrollFlag = true;
        }, 200);
    };
}

const scrolling = sendScrollToPort();

app.ports.datepickerData.subscribe(getDateRange);
app.ports.beforePageLeave.subscribe(() => {
    const dateRangePicker = $("#reportrange").data("daterangepicker"),
        dateMonthPicker = $("#reportrange").data("datepicker");

    dateRangePicker && dateRangePicker.remove();
    dateMonthPicker && dateMonthPicker.destroy();

    $(".scrolling-table-holder").each(function() {
        $(this.querySelector(".scrolling-table-body")).off("scroll", scrolling);
    });
});

app.ports.pageLoad.subscribe(() => {
    setTimeout(() => {
        $(".scrolling-table-holder").each(function() {
            const body = this.querySelector(".scrolling-table-body"),
                sidebar = this.querySelector(".scrolling-table-sidebar"),
                header = this.querySelector(".scrolling-table-header .scrolling-table"),
                footer = this.querySelector(".scrolling-table-footer .scrolling-table");

            sidebar && new SyncScroll(body, sidebar, { axisX: false }).init();
            header && new SyncScroll(body, header, { axisY: false }).init();
            footer && new SyncScroll(body, footer, { axisY: false }).init();

            $(body).on("scroll", scrolling);
        });
    }, 10);
});

app.ports.collapsibleExpand.subscribe(id => {
    const section = $("[data-cid='cid" + id + "']"),
        collapsible = section.find(".collapsible-content"),
        height = collapsible.find("table").outerHeight();

    collapsible.animate({ height: height }, () => {
        collapsible.css("height", "auto");
    });
});

app.ports.collapsibleCollapse.subscribe(id => {
    const section = $("[data-cid='cid" + id + "']"),
        collapsible = section.find(".collapsible-content"),
        height = collapsible.find("table").outerHeight();

    collapsible.animate({ height: 0 });
});
