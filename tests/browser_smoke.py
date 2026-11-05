"""End-to-end regression checks with synthetic data only; no remote deployment."""
import csv
import io
import os
import re
import shutil
import tempfile
from datetime import datetime
from pathlib import Path

from playwright.sync_api import sync_playwright, expect

ROOT = Path(__file__).resolve().parents[1]
ARTIFACTS = Path(os.environ.get("VRAGR_ARTIFACTS", ROOT / "tests/artifacts"))
URL = os.environ.get("VRAGR_TEST_URL", "http://127.0.0.1:3838")


def check_item_indicators(browser, errors):
    page = browser.new_page(viewport={"width": 1440, "height": 1000})
    page.on("pageerror", lambda error: errors.append(str(error)))
    page.goto(URL, wait_until="networkidle")

    def choose(id_, value):
        page.wait_for_function("id => {const x=document.getElementById(id);return x && (x.selectize || x.options)}", arg=id_)
        page.evaluate("""([id,value]) => {const x=document.getElementById(id);
          if(x.selectize) x.selectize.setValue(value); else {x.value=value;x.dispatchEvent(new Event('change',{bubbles:true}));}}""", [id_, value])

    def header(item):
        return page.locator("#single_items .accordion-button").filter(has_text=f"Item {item} ·")

    def badge(item, state, text=None):
        indicator = page.locator(f"#single_item_status_{item} .case-item-status")
        expect(indicator).to_be_visible()
        expect(indicator).to_have_attribute("data-state", state)
        if text:
            expect(indicator).to_contain_text(text)

    def collapse_all():
        expect(page.locator("#single_items .collapsing")).to_have_count(0)
        for button in page.locator("#single_items .accordion-button").all():
            if button.get_attribute("aria-expanded") == "true":
                button.click()
        expect(page.locator("#single_items .collapsing")).to_have_count(0)
        expect(page.locator('#single_items .accordion-button[aria-expanded="true"]')).to_have_count(0)
        expect(page.locator("#single_items .accordion-collapse.show")).to_have_count(0)

    collapse_all()
    for item in range(1, 13):
        badge(item, "missing")
    badge(3, "missing", "6 of 6 missing")
    badge(5, "missing", "27 of 27 missing")
    badge(12, "missing", "5 of 5 missing")
    page.evaluate("window.caseHeaders = Array.from(document.querySelectorAll('#single_items .accordion-button'))")

    header(3).click()
    choose("single_Item_3_1", "No")
    collapse_all()
    badge(3, "partial", "5 of 6 missing · Scoreable")
    header(12).click()
    for field in range(1, 5):
        choose(f"single_Item_12_{field}", "0")
    collapse_all()
    badge(12, "partial", "1 of 5 missing · Scoreable")
    choose("single_Item_12_4", "__missing__")
    badge(12, "missing", "2 of 5 missing · Item missing")
    choose("single_Item_12_4", "0")
    badge(12, "partial", "1 of 5 missing · Scoreable")

    # Keep an English desktop/mobile record with every item closed.
    for width in (390, 1440):
        page.set_viewport_size({"width": width, "height": 1000})
        page.screenshot(path=str(ARTIFACTS / f"item-status-collapsed-en-{width}.png"), full_page=True)

    header(5).click()
    choose("single_info_5", "known")
    badge(5, "complete", "Complete")
    page.locator("#single_Item_5_1").fill("-1")
    collapse_all()
    badge(5, "invalid", "Check inputs")
    badge(3, "partial", "Scoreable")
    choose("single_info_5", "missing")
    badge(5, "missing", "27 of 27 missing")
    choose("single_info_5", "known")
    badge(5, "invalid")
    header(5).click()
    page.locator("#single_Item_5_1").fill("")
    collapse_all()
    badge(5, "invalid", "1 of 27 missing")
    header(5).click()
    page.locator("#single_Item_5_1").fill("0")
    collapse_all()
    badge(5, "complete")

    page.locator("#language").select_option("de")
    badge(3, "partial", "5 von 6 fehlen · Auswertbar")
    badge(12, "partial", "1 von 5 fehlen · Auswertbar")
    expect(page.locator('#single_items .accordion-button[aria-expanded="true"]')).to_have_count(0)
    assert page.evaluate("window.caseHeaders.every((x,i)=>x === document.querySelectorAll('#single_items .accordion-button')[i])")
    for width in (390, 768, 1280, 1440):
        page.set_viewport_size({"width": width, "height": 1000})
        assert page.evaluate("document.documentElement.scrollWidth <= innerWidth + 1")
        clipped = page.locator(".case-item-status").evaluate_all("""nodes => nodes.filter(x => {
          const r=x.getBoundingClientRect(),p=x.closest('.case-item-header').getBoundingClientRect();
          return x.scrollWidth>x.clientWidth+1 || r.right>p.right+1;
        }).map(x=>x.textContent)""")
        assert not clipped, clipped
        if width in (390, 1440):
            page.screenshot(path=str(ARTIFACTS / f"item-status-collapsed-de-{width}.png"), full_page=True)

    page.locator("#single_example").click()
    page.wait_for_function("document.getElementById('single_Item_7').value === '28'")
    badge(7, "complete")
    badge(9, "missing")
    badge(10, "missing", "12 von 12 fehlen")
    page.locator("#single_calculate").click()
    expect(page.locator("#score_value")).to_have_text("14")
    page.locator("#language").select_option("en")
    badge(10, "missing", "12 of 12 missing")
    expect(page.locator("#score_value")).to_have_text("14")
    page.locator("#single_reset").click()
    for item in range(1, 13):
        badge(item, "missing")
    expect(page.locator("#score_value")).to_have_count(0)
    expect(page.locator('#single_items .accordion-button[aria-expanded="true"]')).to_have_count(0)
    page.close()


def run():
    ARTIFACTS.mkdir(parents=True, exist_ok=True)
    errors = []
    with sync_playwright() as p:
        browser = p.chromium.launch(
            executable_path=os.environ.get("CHROMIUM_PATH") or shutil.which("chromium"),
            headless=True, args=["--no-sandbox", "--disable-dev-shm-usage"])
        page = browser.new_page(viewport={"width": 1440, "height": 1000}, accept_downloads=True)
        page.on("pageerror", lambda error: errors.append(str(error)))
        page.goto(URL, wait_until="networkidle")
        check_item_indicators(browser, errors)

        def choose(id_, value):
            page.wait_for_function("id => {const x=document.getElementById(id);return x && (x.selectize || x.options)}", arg=id_)
            page.evaluate("""([id,value]) => {const x=document.getElementById(id);
              if(x.selectize) x.selectize.setValue(value); else {x.value=value;x.dispatchEvent(new Event('change',{bubbles:true}));}}""", [id_, value])

        def language(value):
            page.locator("#language").select_option(value)
            expect(page.locator("html")).to_have_attribute("lang", value)
            expect(page).to_have_title("VRAG-R · Rechner" if value == "de" else "VRAG-R · Calculator")

        def download(control, label="Results"):
            with page.expect_download() as info:
                page.locator(f"#{control}").click()
            file = ARTIFACTS / info.value.suggested_filename
            info.value.save_as(file)
            data = file.read_bytes()
            rows = list(csv.DictReader(io.StringIO(data.decode("utf-8-sig"))))
            time = datetime.strptime(rows[0]["Calculated_at_UTC"], "%Y-%m-%dT%H:%M:%SZ")
            assert info.value.suggested_filename == f"VRAGR_{label}_{time:%Y-%m-%d_%H-%M-%S}_UTC.csv"
            return data, rows, info.value.suggested_filename

        def check_controls():
            page.wait_for_function("""() => Array.from(document.querySelectorAll('select[data-i18n-options]')).every(x => {
              const c=x.selectize; return !c || (c.items.length ? !c.$control_input.attr('placeholder') :
                c.$control_input.attr('placeholder')===c.settings.placeholder);})""")
            clipped = page.locator(".selectize-input .item").evaluate_all("""nodes => nodes.filter(x => {
              const r=x.getBoundingClientRect();if(!r.width || !r.height)return false;
              const p=x.parentElement.getBoundingClientRect();return x.scrollWidth>x.clientWidth+1 || r.right>p.right-18;
            }).map(x=>x.textContent)""")
            assert not clipped, clipped
            assert page.evaluate("document.documentElement.scrollWidth <= innerWidth + 1")

        def check_case_defaults():
            page.wait_for_function("""() => {
              const choices=Array.from(document.querySelectorAll('select[id^="single_Item_"]'));
              const info=[5,7,8,9].map(i=>document.getElementById('single_info_'+i));
              const counts=Array.from(document.querySelectorAll('input[id^="single_Item_5_"],input[id^="single_Item_8_"]'));
              return choices.length===28 && choices.every(x=>x.value==='__missing__') &&
                info.every(x=>x.value==='missing') && counts.length===45 && counts.every(x=>x.value==='0');
            }""")

        expect(page.locator("#language")).to_have_value("en")
        expect(page.locator("#score_value")).to_have_count(0)
        expect(page.locator('a[data-value="explore"]')).to_have_count(0)
        expect(page.locator(".brand-symbol")).to_have_count(0)
        expect(page.locator(".brand-mark")).to_have_attribute("aria-hidden", "true")
        check_case_defaults()
        page.locator("#single_calculate").click()
        expect(page.locator("#single_validation")).to_contain_text("More than four")
        expect(page.locator("#score_value")).to_have_count(0)
        page.locator("#single_items .accordion-button").filter(has_text="Item 5 ·").click()
        choose("single_info_5", "known")
        expect(page.locator("#single_Item_5_1")).to_be_visible()
        expect(page.locator("#single_Item_5_1")).to_have_value("0")
        page.locator("#single_Item_5_1").fill("2")
        choose("single_info_5", "missing")
        expect(page.locator("#single_Item_5_1")).not_to_be_visible()
        choose("single_info_5", "known")
        expect(page.locator("#single_Item_5_1")).to_have_value("2")
        language("de")
        expect(page.locator("#single_Item_5_1")).to_have_value("2")
        language("en")
        page.locator("#single_reset").click()
        check_case_defaults()
        page.locator("#single_items .accordion-button").filter(has_text="Item 5 ·").click()
        check_controls()
        brand = page.locator(".navbar-brand").bounding_box()
        first_tab = page.locator('a[data-value="single"]').bounding_box()
        assert first_tab["x"] - brand["x"] - brand["width"] >= 24
        page.locator("#single_example").click()
        # Wait for the input update, not only the example notice.
        page.wait_for_function("document.getElementById('single_Item_7').value === '28'")
        page.locator("#single_calculate").click()
        expect(page.locator("#score_value")).to_have_text("14")
        expect(page.locator("#category_value")).to_have_text("7")
        expect(page.locator("#prob_five")).to_have_text("45.0 %")
        expect(page.locator("#prob_twelve")).to_have_text("69.0 %")
        expect(page.locator("#single_plot img")).to_be_visible()
        original, rows, name = download("single_download", "Single_case")
        assert rows[0]["Item_7"] == "28" and rows[0]["Prob_12_years"] == "0.69"
        choose("single_plot_type", "normal")
        expect(page.locator("#single_plot_caption")).to_contain_text("not a fitted distribution")
        expect(page.locator("#single_plot img")).to_be_visible()
        normal_csv, _, normal_name = download("single_download", "Single_case")
        assert normal_csv == original and normal_name == name
        page.locator(".result-column .accordion-button").filter(has_text="Download plot").click()
        for format_ in ("png", "jpeg", "pdf", "svg", "tiff"):
            choose("plot_format", format_)
            page.wait_for_timeout(150)  # Complete the Shiny input round trip before HTTP download.
            with page.expect_download() as info:
                page.locator("#plot_download").click()
            assert info.value.suggested_filename.endswith("." + format_)
            plot_file = ARTIFACTS / info.value.suggested_filename
            info.value.save_as(plot_file)
            assert plot_file.stat().st_size > 1000
        page.screenshot(path=str(ARTIFACTS / "single-en.png"), full_page=True)
        for value in ("de", "en", "de"):
            language(value)
            expect(page.locator("#score_value")).to_have_text("14")
            expect(page.locator("#single_plot_type")).to_have_value("normal")
            expect(page.locator("#prob_five")).to_have_text("45,0 %" if value == "de" else "45.0 %")
            page.wait_for_function("expected => {const c=document.getElementById('single_Item_1').selectize;return c.options.No[c.settings.labelField] === expected;}", arg="Nein" if value == "de" else "No")
            repeated, _, repeated_name = download("single_download", "Single_case")
            assert repeated == original and repeated_name == name
            check_controls()
        page.screenshot(path=str(ARTIFACTS / "single-de.png"), full_page=True)
        expect(page.locator("#single_plot_caption")).to_contain_text("tatsächliche Verteilung")
        choose("single_plot_type", "empirical")
        expect(page.locator("#single_plot_caption")).to_contain_text("2017")
        same_csv, _, same_name = download("single_download", "Single_case")
        assert same_csv == original and same_name == name
        page.screenshot(path=str(ARTIFACTS / "single-empirical-de.png"), full_page=True)
        # A direct request to an old download URL is guarded server-side, too.
        old_url = page.locator("#single_download").get_attribute("href")
        choose("single_Item_1", "Yes")
        expect(page.locator("#score_value")).to_have_count(0)
        response = page.request.get(URL + "/" + old_url.lstrip("/"))
        assert response.status != 200 or "Calculated_at_UTC" not in response.text()
        language("en")
        page.locator("#single_calculate").click()
        expect(page.locator("#score_value")).to_have_text("9")

        page.locator('a[data-value="batch"]').click()
        page.locator("#batch_file").set_input_files(ROOT / "examples/example-data.csv")
        expect(page.locator("#batch_preview")).to_contain_text("001")
        expect(page.locator("#batch_validation_summary")).to_contain_text("3 scoreable rows")
        # Check later mapping columns, not just the first input.
        page.wait_for_function("document.getElementById('column_Item_12_5')?.value === 'Item_12_5'")
        choose("batch_id", "Case_ID")
        page.locator("#batch_calculate").click()
        expect(page.locator("#batch_result_preview")).to_contain_text("46")
        batch, batch_rows, batch_name = download("batch_download")
        assert [row["Case_ID"] for row in batch_rows] == ["001", "002", "003"]
        assert [row["Score"] for row in batch_rows] == ["14", "-34", "46"]
        for value in ("de", "en", "de"):
            language(value)
            again, _, same_name = download("batch_download")
            assert again == batch and same_name == batch_name
            check_controls()
        page.screenshot(path=str(ARTIFACTS / "dataset-de.png"), full_page=True)
        choose("batch_decimal", ",")
        expect(page.locator("#batch_download")).to_have_count(0)
        expect(page.locator("#batch_status")).to_contain_text("nicht mehr aktuell")
        language("en")

        # Custom values which match translation keys and very long raw headers.
        with tempfile.TemporaryDirectory() as directory:
            custom = Path(directory) / "custom.csv"
            source = list(csv.DictReader((ROOT / "examples/example-data.csv").open()))
            long_name = "Male_" + "unbroken_raw_column_" * 12
            for row in source:
                row[long_name] = "Male"
                del row["Item_1"]
            with custom.open("w", newline="") as f:
                writer = csv.DictWriter(f, fieldnames=list(source[0])); writer.writeheader(); writer.writerows(source)
            custom_bytes = custom.read_bytes()
            page.locator("#batch_file").set_input_files(custom)
            # Wait for the new file's unique header before touching dynamic inputs.
            page.wait_for_function("name => !!document.getElementById('column_Item_1')?.selectize?.options[name]", arg=long_name)
            choose("column_Item_1", long_name)
            page.wait_for_function("document.querySelectorAll('#category_mapping select').length > 0 && Array.from(document.querySelectorAll('#category_mapping label')).some(x=>x.textContent.includes('Male'))")
            mapping_id = page.locator("#category_mapping .shiny-input-container").filter(has=page.locator("label", has_text="Male")).locator("select").first.get_attribute("id")
            choose(mapping_id, "Yes")
            expect(page.locator("#batch_validation_summary")).to_contain_text("3 scoreable rows")
            # Expand the first column and category accordion to inspect selected labels.
            page.locator("#column_mapping .accordion-button").first.click()
            page.locator("#category_mapping .accordion-button").first.click()
            for value in ("de", "en", "de"):
                language(value);check_controls()
                expect(page.locator("#column_Item_1")).to_have_value(long_name)
                expect(page.locator(f"#{mapping_id}")).to_have_value("Yes")
                expect(page.locator("#batch_preview")).to_contain_text("Male")
                assert "Männlich" not in page.locator("#batch_preview").inner_text()
            page.locator("#batch_calculate").click()
            _, mapped_rows, _ = download("batch_download")
            assert mapped_rows[0]["Item_1"] == "Yes"

        # Invalid rows remain in order, with empty scores and original text IDs.
        with tempfile.TemporaryDirectory() as directory:
            mixed = Path(directory) / "invalid-row.csv"
            source = list(csv.DictReader((ROOT / "examples/example-data.csv").open()))
            source[1]["Item_9"] = "-1"
            with mixed.open("w", newline="") as f:
                writer = csv.DictWriter(f, fieldnames=list(source[0]));writer.writeheader();writer.writerows(source)
            page.locator("#batch_file").set_input_files(mixed)
            expect(page.locator("#batch_issues")).to_contain_text("Item_9")
            choose("batch_id", "Case_ID")
            page.locator("#batch_calculate").click()
            expect(page.locator("#batch_status")).to_contain_text("invalid-row.csv")
            _, mixed_rows, _ = download("batch_download")
            assert mixed_rows[1]["Case_ID"] == "002"
            assert mixed_rows[1]["Status"] == "not_calculable" and mixed_rows[1]["Score"] == ""
            # Restore the long-header case for responsive layout regression checks.
            page.locator("#batch_file").set_input_files({"name": "custom-again.csv", "mimeType": "text/csv", "buffer": custom_bytes})
            page.wait_for_function("name => !!document.getElementById('column_Item_1')?.selectize?.options[name]", arg=long_name)
            choose("column_Item_1", long_name)
            page.locator("#column_mapping .accordion-button").first.click()
            page.locator("#category_mapping .accordion-button").first.click()

        # Independent sessions start in English with explicit missing defaults.
        other = browser.new_page()
        other.goto(URL, wait_until="networkidle")
        expect(other.locator("#language")).to_have_value("en")
        expect(other.locator("#score_value")).to_have_count(0)
        expect(other.locator("#single_Item_1")).to_have_value("__missing__")
        expect(other.locator("#single_info_5")).to_have_value("missing")
        other.close()
        page.locator('a[data-value="methods"]').click()
        expect(page.locator('a[data-value="methods"]')).to_have_text("Methodische Hinweise")
        expect(page.locator(".tab-pane.active")).to_contain_text("Originalpublikation")
        page.locator('a[data-value="about"]').click()
        expect(page.locator(".tab-pane.active")).to_contain_text("OpenAI Codex")
        expect(page.locator(".tab-pane.active")).not_to_contain_text("Die KI-Unterstützung ersetzt")
        expect(page.locator(".tab-pane.active")).to_contain_text("Server-Protokollen")
        expect(page.locator(".tab-pane.active")).to_contain_text("Verarbeiten Sie nur Daten, die sich keiner Person zuordnen lassen.")
        expect(page.get_by_role("link", name="Posit-Connect-Cloud-Dokumentation zur Sicherheit")).to_have_attribute("href", "https://docs.posit.co/connect-cloud/user/platform/security.html")
        page.screenshot(path=str(ARTIFACTS / "about-de.png"),full_page=True)
        language("en")
        expect(page.locator(".tab-pane.active")).to_contain_text("Use only data that cannot be linked to an individual.")
        expect(page.locator(".tab-pane.active")).to_contain_text("us-east-2 (USA)")
        expect(page.get_by_role("link", name="Posit privacy policy", exact=True)).to_have_attribute("href", "https://posit.co/about/privacy-policy/")
        expect(page.get_by_role("link", name="Posit service terms", exact=True)).to_have_attribute("href", "https://posit.co/about/posit-service-terms-of-use/")
        page.screenshot(path=str(ARTIFACTS / "about-en.png"),full_page=True)
        language("de")
        page.locator('a[data-value="batch"]').click()
        for width in (390, 768, 1024, 1280, 1440, 1920):
            page.set_viewport_size({"width": width, "height": 1000})
            check_controls()
            if width == 390:
                page.locator("button.navbar-toggle, button.navbar-toggler").click()
                expect(page.locator(".collapsing")).to_have_count(0)
                page.locator("#language").scroll_into_view_if_needed()
                expect(page.locator("#language")).to_be_visible()
                page.screenshot(path=str(ARTIFACTS / "mobile-navigation-de.png"), full_page=False)
            if width in (390, 1024, 1920):
                page.screenshot(path=str(ARTIFACTS / f"dataset-de-{width}.png"), full_page=True)
        assert not errors, errors
        browser.close()
    print("Browser checks passed: missing/zero defaults, EN/DE, both plots and five formats, source/disclaimer text, independent sessions, calculation, mappings, stable/stale exports and six viewport widths.")


if __name__ == "__main__":
    run()
