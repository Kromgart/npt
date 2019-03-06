import $ from "jquery";
import moment from "moment";
import datepicker from "bootstrap-datepicker";

import "./styles.scss";

const createMonthPicker = ($elem, dateRange, publishUpdate) => {
    if ($elem.data("datepicker")) {
        $elem.datepicker("update", new Date(dateRange.startDate.replace(/\<?(.+?)\>/, "$1")));
        return;
    }
    const format = "MM yyyy";
    const picker = $elem.datepicker(
        Object.assign(
            {},
            {
                format,
                minViewMode: 1,
                maxViewMode: 2,
                autoclose: true,
                updateViewDate: true,
                todayBtn: "linked"
            }
        )
    );

    $(".fake-report").on("click", () => {
        $elem.trigger("focus");
    });

    picker.datepicker("update", new Date(dateRange.startDate.replace(/\<?(.+?)\>/, "$1")));

    $elem.on("changeDate.datepicker", function(e) {
        // https://github.com/elm-community/elm-time/issues/21
        const iso = d => d.toISOString().replace(".000Z", "Z");
        publishUpdate(this, {
            startDate: iso(
                moment(e.date)
                    .startOf("month")
                    .toDate()
            ),
            endDate: iso(
                moment(e.date)
                    .endOf("month")
                    .toDate()
            )
        });
    });
};

export default createMonthPicker;
