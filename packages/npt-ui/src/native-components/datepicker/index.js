import $ from "jquery";
import moment from "moment";
import daterangepicker from "daterangepicker";

import "./styles.scss";

const createDatePicker = ($elem, dateRange, publishUpdate) => {
    const format = "DD.MM.YYYY";
    const startEnd = dateRange
        ? {
              startDate: moment(new Date(dateRange.startDate.replace(/\<?(.+?)\>/, "$1")), format),
              endDate: moment(new Date(dateRange.endDate.replace(/\<?(.+?)\>/, "$1")), format)
          }
        : {};

    const picker = $elem.daterangepicker(
        Object.assign({}, startEnd, {
            autoUpdateInput: false,
            applyClass: "btn-primary",
            todayBtn: true,
            locale: {
                format,
                cancelLabel: "Clear"
            }
        })
    );

    $elem.on("show.daterangepicker", (e, picker) => {
        picker.container.on("click", "th.month", function(e) {
            const startDate = moment(e.target.innerHTML, "MMM YYYY"),
                endDate = moment(startDate).endOf("month");
            picker.setStartDate(startDate);
            picker.setEndDate(endDate);
            picker.updateView();
        });
    });

    $elem.on("cancel.daterangepicker", function() {
        publishUpdate(this, null);
    });

    $elem.on("apply.daterangepicker", function(e, picker) {
        // https://github.com/elm-community/elm-time/issues/21
        const iso = d => d.toISOString().replace(".000Z", "Z");
        publishUpdate(this, {
            startDate: iso(picker.startDate),
            endDate: iso(picker.endDate)
        });
    });
};

export default createDatePicker;
