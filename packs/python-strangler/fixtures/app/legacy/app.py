"""The Flask app: every route, wired by hand to a view."""

from flask import Flask, Response, jsonify, request

from legacy import db, reports, views


def respond(result):
    if isinstance(result, tuple):
        body, status = result
        return jsonify(body), status
    return jsonify(result)


def create_app(seed=True):
    app = Flask(__name__)
    db.conn()
    if seed:
        db.seed()

    @app.get("/bookings")
    def bookings_today():
        return respond(views.todays_bookings(request.args.get("day")))

    @app.get("/bookings/<booking_id>")
    def booking(booking_id):
        return respond(views.booking(booking_id))

    @app.post("/bookings")
    def make_booking():
        return respond(views.make_booking(request.get_json(silent=True) or {}))

    @app.post("/bookings/form")
    def form_booking():
        return respond(views.form_booking(request))

    @app.post("/bookings/<booking_id>/cancel")
    def cancel_booking(booking_id):
        body = request.get_json(silent=True) or {}
        return respond(views.cancel_booking(booking_id, body.get("reason")))

    @app.post("/bookings/<booking_id>/move")
    def move_booking(booking_id):
        return respond(views.move_booking(booking_id, (request.get_json(silent=True) or {}).get("start", "")))

    @app.post("/bookings/<booking_id>/no-show")
    def no_show(booking_id):
        return respond(views.mark_no_show(booking_id))

    @app.get("/tables")
    def tables():
        return respond(views.tables_view(request.args.get("area")))

    @app.post("/tables/<table_id>/close")
    def close_table(table_id):
        return respond(views.close_table(table_id))

    @app.post("/tables/<table_id>/open")
    def open_table(table_id):
        return respond(views.open_table(table_id))

    @app.post("/seat")
    def seat():
        body = request.get_json(silent=True) or {}
        return respond(views.seat_party(body.get("party"), body.get("start"), body.get("area")))

    @app.post("/queue")
    def queue():
        body = request.get_json(silent=True) or {}
        return respond(views.add_to_queue(body.get("guest", ""), body.get("party", 1)))

    @app.get("/dashboard")
    def dashboard():
        return respond(views.dashboard())

    @app.get("/export.csv")
    def export():
        return Response(views.export_csv(request.args.get("day")), mimetype="text/csv")

    @app.get("/reports/week")
    def week():
        return respond(reports.week_summary(request.args.get("from")))

    @app.get("/reports/no-shows")
    def no_shows():
        return respond(reports.no_shows())

    return app


if __name__ == "__main__":
    create_app().run(debug=True)
