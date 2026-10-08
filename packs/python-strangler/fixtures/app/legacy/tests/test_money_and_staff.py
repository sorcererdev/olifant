"""Payments and the rota, tested after the fact."""

from datetime import date, timedelta

import pytest

from legacy import db, payments, staff, utils, views


@pytest.fixture(autouse=True)
def fresh():
    db.reset()
    db.seed()
    utils.use_restaurant("bistro")
    staff.SHIFTS.clear()
    staff.TIPS.clear()
    yield


def big_booking(days_ahead=3):
    day = (date.today() + timedelta(days=days_ahead)).isoformat()
    body, status = views.make_booking({"guest": "Barbara", "party": "6", "start": "21:00", "day": day})
    assert status == 201
    return body["id"]


def test_a_deposit_is_charged_once():
    booking_id = big_booking()
    assert payments.take_deposit(booking_id, "4242424242424242")["amount"] == 60
    assert payments.take_deposit(booking_id, "4242424242424242")[1] == 409


def test_a_declined_card_is_recorded_and_refused():
    booking_id = big_booking()
    body, status = payments.take_deposit(booking_id, "4000000000000002")
    assert status == 402
    assert payments.payments_for(booking_id)[0]["status"] == "failed"


def test_a_small_party_pays_no_deposit():
    assert payments.take_deposit("b1", "4242424242424242") == ({"error": "no deposit for this party"}, 400)


def test_a_refund_in_time_is_paid_back():
    booking_id = big_booking()
    payments.take_deposit(booking_id, "4242424242424242")
    assert payments.refund_deposit(booking_id)["refunded"] == 60


def test_a_refund_on_the_day_is_too_late():
    booking_id = big_booking(days_ahead=0)
    payments.take_deposit(booking_id, "4242424242424242")
    assert payments.refund_deposit(booking_id)[1] == 409


def test_the_till_nets_refunds():
    booking_id = big_booking()
    payments.take_deposit(booking_id, "4242424242424242")
    payments.refund_deposit(booking_id)
    assert payments.till_total()["net"] == 0


def test_shifts_may_not_overlap():
    assert staff.add_shift("ben", "today", "17:00", "23:00")["staff"] == "ben"
    assert staff.add_shift("ben", "today", "22:00", "23:30")[1] == 409


def test_who_is_on_shift():
    staff.add_shift("ana", "today", "17:00", "22:00")
    staff.add_shift("dee", "today", "16:00", "23:00")
    assert staff.on_shift("today", "18:00") == ["ana", "dee"]


def test_tips_split_by_hours_with_the_chef_on_half():
    staff.add_shift("ben", "today", "18:00", "22:00")
    staff.add_shift("dee", "today", "18:00", "22:00")
    staff.record_tips("today", 90)
    assert staff.split_tips("today") == {"ben": 60.0, "dee": 30.0}


def test_pay_for_the_week():
    monday = date.today() - timedelta(days=date.today().weekday())
    staff.add_shift("cy", monday.isoformat(), "17:00", "23:00")
    assert staff.pay("cy")["hours"] == 6.0
